#setwd("~/Desktop/Niche")
setwd("/media/eric/Lockbox/GitHub/Niche/")

##Load packages
library(sf)
library(raster)
library(sp)
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(rgdal)
library(raster)
library(caTools)
library(maptools)
library(plotly)
library(maps)
library(rgeos)
library(geosphere)
library(jsonlite)

# #--(0) Universal variables
p4s_ll <- CRS("+proj=longlat")
## Variable transformation function
transform_data <- function(x) {
  min_x = min(x, na.rm = TRUE)
  max_x = max(x, na.rm = TRUE)
  mean_x = mean(x, na.rm = TRUE)
  y = (x - mean_x) / (max_x - min_x)
  return(y)
}


#--(1) Get county-level boundary data, the unit of analysis

county_df <- map_data("county") %>%
  mutate(region = tolower(region)) %>%
  mutate(subregion = tolower(subregion)) %>%
  mutate(subregion = str_replace(subregion, "st ", "saint ")) %>%
  mutate(subregion = str_replace(subregion, "ste ", "saint ")) %>%
  mutate(region = str_replace(region, "\\s", "_")) %>%
  mutate(subregion = str_replace(subregion, "\\s", "_")) %>%
  mutate(id = paste(subregion, region, sep=",_"))
coordinates(county_df) = c("long","lat") #converts to SpatialPointsDataFrame

# #convert to SpatialPolygonsDataFrame
# #https://stackoverflow.com/questions/21759134/convert-spatialpointsdataframe-to-spatialpolygons-in-r
# points2polygons <- function(df,data) {
#   get.grpPoly <- function(group,ID,df) {
#     Polygon(coordinates(df[df$id==ID & df$group==group,]))
#   }
#   get.spPoly  <- function(ID,df) {
#     Polygons(lapply(unique(df[df$id==ID,]$group),get.grpPoly,ID,df),ID)
#   }
#   spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
#   SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
# }
# 
# sp_dat = county_df$id %>%
#   unique() %>%
#   factor() %>%
#   data.frame(name=.)
# rownames(sp_dat) = sp_dat$name
# sp_dat$id = as.numeric(sp_dat$name)
# 
# counties <- points2polygons(county_df, sp_dat) #this may take a moment
# locs = counties$name %>%
#   as.character() %>%
#   strsplit(split = ",_") %>%
#   do.call("rbind", .)
# counties$county = locs[,1]
# counties$state = locs[,2]
#  
# writePolyShape(counties, "raw_data/shape/counties.shp")
d <- d2 <- st_read("raw_data/shape/counties.shp", quiet = TRUE)
counties <- as(d,"Spatial")
proj4string(counties) <- st_crs(d) <- p4s_ll
d2$geometry <- NULL
#
#--(1) Climatric variables from WorldClim
# #Download environmental variables
# getData('worldclim', var='bio', res=5, path = "raw_data/") #35 MB download
# varsToGet = c("tmean","tmin","tmax","prec","bio","alt")
# sapply(varsToGet, function(x) getData(name = "worldclim", download = TRUE, res=10, var=x))

#bioclim variables (https://worldclim.org/raw_data/bioclim.html)
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation × 100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter
#
bclimFiles = list.files("raw_data/wc2-5/", pattern = "bio", full.names = TRUE)[-1]
bclimFiles = bclimFiles[str_detect(bclimFiles, "bil")==TRUE]
orderFiles = str_extract_all(bclimFiles, pattern="[[:digit:]]+") %>%
  do.call("rbind",.) %>%
  as.matrix %>%
  apply(., 2, as.numeric)
orderFiles = order(orderFiles[,2])
bioclim = stack(bclimFiles[orderFiles]) %>%
  crop(., extent(d) + c(-1,1) + 2)
ex2 = raster::extract(bioclim[[c(1,12,15)]], counties, fun="mean") %>% 
  data.frame() %>%
  round(2)
names(ex2) = c("Mean_Temp_F","Precipitation_mm","Precip_Variation")
d2 = cbind(d2, ex2)

plot(bioclim, col=viridis::viridis(15))

## Use principal components to do feature extraction
#normalize data to mean 0 and unit variance
normalize <- function(x){
  mu <- mean(x, na.rm=TRUE)
  sigma <- sd(x, na.rm=TRUE)
  y <- (x-mu)/sigma
  return(y)
}

i = 1:19
vals = getValues(bioclim[[i]])
vals2 <- apply(vals, 2, normalize)
completeObs = complete.cases(vals2)

pc = princomp(vals2[completeObs,], scores = TRUE, cor = TRUE)

pcPredict = predict(pc, vals)
r1 = r2 = r3 = raster(bioclim)
r1[] = pcPredict[,1]
r2[] = pcPredict[,2]
r3[] = pcPredict[,3]
pcClim = stack(r1,r2,r3)

names(pcClim) = c("CLIM_PC1","CLIM_PC2","CLIM_PC3")
saveRDS(pcClim, file = "raw_data/bioclim_pca_rasters.rds")
pcClim = readRDS("raw_data/bioclim_pca_rasters.rds")

bioclim_ex = raster::extract(
  pcClim,
  d,
  fun = mean
)

d = cbind(d, bioclim_ex)
#
# #--(2) WDPA data for recreation and land conservation data
#
# # https://www.iucn.org/theme/protected-areas/about/protected-area-categories
# ##Subset / clean data
# #read into workspace
# wdpa = readOGR("raw_data/WDPA/WDPA_Apr2020_USA-shapefile-polygons.shp")
# #PAs in contiguous United States
# rmPA = c("US-AK","US-N/A;US-AK","US-N/A","US-HI","US-N/A;US-AK","US-N/A;US-CA","US-N/A;US-FL","US-N/A;US-FL;US-GA;US-SC","US-N/A;US-HI")
# wdpa_48 = wdpa[!wdpa$SUB_LOC %in% rmPA,]
# #Only terrestrial parks?
# wdpa_48_terr = wdpa_48[wdpa_48$MARINE==0,]
# #Subset by PA type
# paLevels = c("Ib","II","III","V","VI")
# wdpa_48_cat = wdpa[!wdpa$IUCN_CAT %in% rmPA,]
# writeSpatialShape(protectedAreas_pa, fn = "raw_data/WDPA/Derived/public_lands_recreation.shp", factor2char = TRUE)
#
protectedAreas = readOGR("raw_data/WDPA/Derived/public_lands_recreation.shp",
                         p4s = proj4string(counties)
)
paLevels = c("Ib","II","III","V","VI")

#Ia Strict Nature Reserve
#Ib Wilderness Area
#II National Park
#III Natural Monument or Feature
#IV Habitat/Species Management Area
#V Protected Landscape/ Seascape
#VI Protected Landscape + Wildlife Resto.

#for each county polygon, find the proportion representation of each PA type
#parallelize for speed
library(foreach)
library(doMC)

registerDoMC(cores = 4)
index = 1:nrow(counties)
outMatrix = foreach(i=index) %dopar% {
  countyPoly = gBuffer(counties[i,], byid = TRUE, width = 0) #fixes topology error (RGEOS)
  z = over(countyPoly, protectedAreas, returnList=TRUE)[[1]]
  if(nrow(z)>0){
    z$IUCN_CAT = factor(z$IUCN_CAT, paLevels)
    z$area = NA
    countyArea = areaPolygon(countyPoly) * 1e-6 #m^2 -> km^2
    for(k in 1:nrow(z)){
      paPoly = protectedAreas[protectedAreas$NAME %in% z$NAME[k],] #%>%
      # gBuffer(., byid = TRUE, width = 0) #fixes topology error (RGEOS)
      z$area[k] = rgeos::gIntersection(countyPoly, paPoly) %>% 
        areaPolygon * 1e-6 #m^2 -> km^2
    }
    with(z, tapply(area, list(IUCN_CAT), function(x) sum(x, na.rm=TRUE))) / countyArea
  } else{
    rep(NA, 7)
  }
  
} %>% 
  do.call("rbind",.) %>% 
  as_tibble

outMatrix[is.na(outMatrix)] <- 0
pa_by_county = data.frame(outMatrix)
names(pa_by_county) <- paste0("PCT_PA_AREA_", paLevels)
pa_by_county$TOT_PA_AREA = rowSums(pa_by_county)

vars = names(pa_by_county)[-which(colSums(pa_by_county)==0)]
for(v in 1:length(vars)){
  d[vars[v]] <- pa_by_county[[vars[v]]] %>% asin()
}
d2$Pct_PublicLands = round(d$TOT_PA_AREA * 100)
#
# #--(3) County level demographic data
#
countyDemo = "raw_data/2018-elections-context.csv" %>% 
  read.csv(stringsAsFactors = FALSE) %>%
  mutate(state = tolower(state)) %>%
  mutate(county = tolower(county)) %>%
  mutate(county = str_replace(county, "st ", "saint ")) %>%
  mutate(county = str_replace(county, "ste ", "saint ")) %>%
  mutate(state = str_replace(state, "\\s", "_")) %>%
  mutate(county = str_replace(county, "\\s", "_")) %>%
  mutate(name = paste(county, state, sep=",_")) %>%
  filter(!duplicated(name)) %>%
  mutate(total_population = log(total_population)) %>%
  mutate(white_pct = asin(white_pct/100)) %>%
  mutate(black_pct = asin(black_pct/100)) %>%
  mutate(hispanic_pct = asin(hispanic_pct/100)) %>%
  mutate(foreignborn_pct = asin(foreignborn_pct/100)) %>%
  mutate(age29andunder_pct = asin(age29andunder_pct/100)) %>%
  mutate(age65andolder_pct = asin(age65andolder_pct/100)) %>%
  mutate(rural_pct = asin(rural_pct/100)) %>%
  mutate(lesshs_pct = asin(lesshs_pct/100))


countyPoverty = "raw_data/2018-county-poverty-SAIPESNC_25AUG20_17_45_39_40.csv" %>%
  read.csv(stringsAsFactors = FALSE) %>%
  mutate(Median_HH_Inc = str_remove_all(Median.Household.Income.in.Dollars, "[\\$|,]")) %>%
  mutate(Median_HH_Inc = as.numeric(Median_HH_Inc)) %>%
  mutate(Median_HH_Inc = log(Median_HH_Inc)) %>%
  mutate(poverty_pct = asin(All.Ages.in.Poverty.Percent/100))

countyAll = countyDemo %>% left_join(countyPoverty, by=c("fips"="County.ID"))

#
##get variables
totalVotes2016 = cbind(countyDemo$trump16,countyDemo$clinton16, countyDemo$otherpres16) %>% rowSums()
countyAll$trump16_pct = (countyDemo$trump16 / totalVotes2016) %>% asin()

totalVotes2012 = cbind(countyDemo$obama12,countyDemo$romney12, countyDemo$otherpres12) %>% rowSums()
countyAll$romney12_pct = (countyDemo$romney12 / totalVotes2012)  %>% asin()

social_vars = c("total_population","white_pct","black_pct","hispanic_pct","foreignborn_pct",
                "age29andunder_pct","age65andolder_pct","rural_pct")
economic_vars = c("Median_HH_Inc","clf_unemploy_pct","poverty_pct","lesscollege_pct","lesshs_pct")
political_vars = c("trump16_pct","romney12_pct")
vars = c(social_vars,economic_vars,political_vars)

vals = countyAll[, vars]
#vals2 = apply(log(vals+1),2, scale)
ii = complete.cases(countyAll[, vars])


pc = princomp(vals2[ii, ], cor = TRUE)
pc$loadings
# Loadings:
#                    Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9 Comp.10 Comp.11 Comp.12 Comp.13
# total_population   0.348         0.281         0.502         0.203  0.282  0.177  0.510   0.325   0.137
# white_pct         -0.271  0.329  0.136  0.194  0.482               -0.311  0.267         -0.490          -0.339
# black_pct          0.201 -0.293  0.357               -0.779 -0.218 -0.181 -0.180  0.130
# hispanic_pct       0.279        -0.572 -0.256  0.121        -0.140 -0.120 -0.244  0.203           0.171  -0.589
# foreignborn_pct    0.362        -0.359 -0.269  0.179        -0.145 -0.188  0.227         -0.344  -0.108   0.620
# trump16_pct       -0.322        -0.326  0.298  0.454 -0.224        -0.200 -0.282 -0.147   0.453   0.109   0.289
# lesscollege_pct   -0.257 -0.385 -0.177  0.112  0.187         0.216  0.490 -0.342  0.225  -0.489
# clf_unemploy_pct   0.104 -0.392  0.311         0.350  0.485 -0.491        -0.230 -0.245          -0.108
# lesshs_pct               -0.504 -0.204         0.191 -0.172  0.144  0.150  0.606 -0.372   0.149  -0.160  -0.203
# age29andunder_pct  0.292 -0.104 -0.142  0.620 -0.144  0.154        -0.227         0.226          -0.584
# age65andolder_pct -0.339               -0.566                0.158 -0.129         0.218   0.214  -0.638
# median_hh_inc      0.170  0.462                      -0.202 -0.360  0.587        -0.275          -0.365
# rural_pct         -0.382 -0.110 -0.123        -0.181        -0.633  0.136  0.344  0.480   0.120

# Importance of components:
#                          Comp.1    Comp.2    Comp.3
# Standard deviation     2.1515116 1.7296844 1.2092481
# Proportion of Variance 0.3560771 0.2301391 0.1124831
# Cumulative Proportion  0.3560771 0.5862162 0.6986993


# demog_scores = matrix(NA, nrow(countyDemo), ncol=3)
# demog_scores[ii,] <- pc$scores[,1:3]
# demog_scores <- data.frame(demog_scores)
# names(demog_scores) = c("DEMO_PC1","DEMO_PC2","DEMO_PC3")
# demog_scores$name = countyDemo$name
#
# d <-
#   d %>%
#   left_join(demog_scores, by="name")

d <- d %>%
  left_join(countyAll[, c("name", vars)], by = "name")


#Summary data
vars2 = c(
  "total_population",
  "Median_HH_Inc",
  "rural_pct",
  "age29andunder_pct",
  "age65andolder_pct"
)
varNames2 = c("Population",
              "Median_Income",
              "Pct_Rural",
              "Pct_Under_29",
              "Pct_Older_65")
countyAll[, vars2] = round(countyAll[, vars2], 2)
newVals = countyAll[, c("name", vars2)]
names(newVals) = c("name", varNames2)
d2 <-
  d2 %>%
  left_join(newVals, by = "name") %>%
  mutate(Pct_Rural = round(100*sin(Pct_Rural))) %>%
  mutate(Pct_Under_29 = round(100*sin(Pct_Under_29))) %>%
  mutate(Pct_Older_65 = round(100*sin(Pct_Older_65)))


## National Land Cover Data
# #https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend
# landUseLevels = c(11:12, 21:24, 31, 41:43, 51:52, 71:74, 81:82, 90, 95)
# nlcd <- "raw_data/NLCD/NLCD_2016_Land_Cover_L48_20190424.img" %>%
#   raster()
# p4s_nlcd <- proj4string(nlcd)
# counties2 <- spTransform(counties, p4s_nlcd) %>%
#   split(., .$id)
# 
# library(foreach)
# library(doMC)
# 
# s = Sys.time()
# registerDoMC(cores = 6)
# index = 1:length(counties2)
# county_state <- sapply(counties2, function(x)
#   x$name) %>%
#   as.character()
# foreach(i = index) %dopar% {
#   #for(i in index){
#   name. <- county_state[i]
#   fileName <-
#     paste0(formatC(i, width = 4, flag = "0"), "_", name.)
#   dirName <- sprintf("land_use/%s", fileName)
#   if (!dir.exists(dirName)) {
#     dir.create(dirName)
#   }
#   fileOut <- sprintf("%s/%s.json", dirName, fileName)
#   if (!file.exists(fileOut)) {
#     #countyPoly = counties2[[i]]
#     countyPoly = gBuffer(counties2[[i]], byid = TRUE, width = 0) #fixes topology error (RGEOS)
#     area. <- (gArea(countyPoly) * 1e-6)
#     if (area. > 1e3) {
#       dims <-
#         ifelse(area. > 2.5e4,
#                6,
#                ifelse(
#                  area. <= 2.5e4 & area. > 1e4,
#                  4,
#                  ifelse(area. > 1e3 & area. <= 1e4, 2, 1)
#                ))
#       grd <-
#         d[d$name == name., ] %>%
#         st_make_valid() %>%
#         sf::st_make_grid(n = dims) %>%
#         st_transform(., crs = st_crs(p4s_nlcd)) %>%
#         as(., 'Spatial') %>%
#         split(., .@plotOrder)
#       grd_list <- lapply(grd, function(g)
#         crop(countyPoly, g))
#       sapply(1:length(grd_list), function(j) {
#         fileName2 <- str_split(fileName, "_", n = 2) %>%
#           unlist()
#         fileName2 <-
#           paste0(fileName2[1], paste0(".", j, "_"), fileName2[2])
#         fileOut2 <- sprintf("%s/%s.json", dirName, fileName2)
#         if (!file.exists(fileOut2)) {
#           countyPoly = gBuffer(grd_list[[j]], byid = TRUE, width = 0) #fixes topology error (RGEOS)
#           nlcd_sub <- crop(nlcd, grd.)
#           cellTypes <- raster::extract(nlcd_sub, grd.) %>%
#             unlist()
#           yf <- factor(cellTypes, landUseLevels) %>%
#             table() %>%
#             #prop.table() %>%
#             data.frame()
#           names(yf) <- c("category", "Count")
#           jsonlite::write_json(yf, path = fileOut2)
#         }
#       })
#     }
#     else{
#       nlcd2 <- crop(nlcd, countyPoly)
#       cellTypes <- raster::extract(nlcd2, countyPoly) %>%
#         unlist()
#       yf <- factor(cellTypes, landUseLevels) %>%
#         table() %>%
#         #prop.table() %>%
#         data.frame()
#       names(yf) <- c("category", "Count")
#       jsonlite::write_json(yf, path = fileOut)
#     }
#   }
# }
# e = Sys.time()
# difftime(e, s, "mins")
# #Time difference of 15.5887 hours

dirs <- list.dirs("land_use/", recursive = TRUE)[-1]
#function for reading and processing the files in each directory (county)
ingest <-  function(x){
  f <- list.files(x, full.names = TRUE)
  d <- lapply(f, function(y){
    z <- read_json(y)
    lapply(z, data.frame) %>% 
      do.call("rbind",.)
  }) %>%
    do.call("rbind",.)
  out <- tapply(d$Count, list(d$category), sum) %>% 
    prop.table()
  return(out)
}

#iterate read and processing function over directories
lc <- lapply(dirs, ingest) %>%
  do.call("rbind",.)
rmCols <- which(attributes(lc)$dimnames[[2]] %in% paste(72:74))

#feature reduction using PCA
pc <- princomp(lc[,-rmCols], scores = TRUE, cor = FALSE)
scores <-pc$scores[,1:4] %>% data.frame()
names(scores) <- vars <- paste0("LandCover_PC",1:4)
for(v in 1:length(vars)){
  d[vars[v]] <- scores[[vars[v]]]
}

## Save data table to storage
saveRDS(d, "county_data.rds")
d2$state = d2$state %>% str_replace_all("_", " ") %>% str_to_title()
d2$county = d2$county %>% str_replace_all("_", " ") %>% str_to_title()
d2$Mean_Temp_F = ((d2$Mean_Temp_F-32) * (5/9)) %>% round(2)
saveRDS(d2[,-c(1:3)], "summary_data.rds")
