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

# #--(1) Get county-level boundary data, the unit of analysis
# 
# county_df <- map_data("county") %>%
#   mutate(region = tolower(region)) %>%
#   mutate(subregion = tolower(subregion)) %>%
#   mutate(subregion = str_replace(subregion, "st ", "saint ")) %>%
#   mutate(subregion = str_replace(subregion, "ste ", "saint ")) %>%
#   mutate(region = str_replace(region, "\\s", "_")) %>%
#   mutate(subregion = str_replace(subregion, "\\s", "_")) %>%
#   mutate(id = paste(subregion, region, sep=",_"))
# coordinates(county_df) = c("long","lat") #converts to SpatialPointsDataFrame
# 
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
counties <- readOGR("raw_data/shape/counties.shp")
d <- d2 <- sf::st_read("raw_data/shape/counties.shp", quiet = TRUE)
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
# BIO4 = Temperature Seasonality (standard deviation ×100)
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
bclimFiles = list.files("raw_data/wc5/", pattern = "bio", full.names = TRUE)[-1]
bclimFiles = bclimFiles[str_detect(bclimFiles, "bil")==TRUE]
orderFiles = str_extract_all(bclimFiles, pattern="[[:digit:]]+") %>%
              do.call("rbind",.) %>%
              as.matrix %>%
              apply(., 2, as.numeric)
orderFiles = order(orderFiles[,2])
bioclim = stack(bclimFiles[orderFiles]) %>%
            crop(., extent(d) + c(-1,1) + 2)
ex2 = raster::extract(bioclim[[c(1,4,12,15)]], counties, fun="mean") %>% 
  data.frame() %>%
  round(2)
names(ex2) = c("Mean_Temp_F","Temp_Variation","Precipitaion_mm","Precip_Variation")
d2 = cbind(d2, ex2)
#rescale raster data to mean 0 and unit variance
for(m in 1:nlayers(bioclim)){
  x = bioclim[[m]][]
  mu = mean(x, na.rm=TRUE)
  sigma = sd(x, na.rm=TRUE)
  bioclim[[m]][] = (x-mu) / sigma
}

plot(bioclim, col=viridis::viridis(15))

#Use principal components to do feature extraction
i = c(1,2,3,4,5,6,10,11,12,15)
vals = getValues(bioclim[[i]])
completeObs = complete.cases(vals)
pc = princomp(vals[completeObs,], scores = TRUE, cor = TRUE)

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
# writeSpatialShape(wdpa_48_terr, fn = "raw_data/WDPA/Derived/public_lands.shp", factor2char = TRUE)
# #Subset by PA type
# keepPAType = c("State Park","National Park","National Monument","State Recreation Area","Recreation Area","Historic State Park","Regional Park")
# wdpa_48_terr_pa = protectedAreas[protectedAreas$DESIG %in% keepPAType,]
# writeSpatialShape(protectedAreas_pa, fn = "raw_data/WDPA/Derived/public_lands_recreation.shp", factor2char = TRUE)
#
protectedAreas = readOGR("raw_data/WDPA/Derived/public_lands_recreation.shp",
                         p4s = proj4string(counties)
)
paLevels = c("Ia","Ib","II","III","IV","V","VI")
#
# #Ia Strict Nature Reserve
# #Ib Wilderness Area
# #II National Park
# #III Natural Monument or Feature
# #IV Habitat/Species Management Area
# #V Protected Landscape/ Seascape
# #VI Protected Landscape + Wildlife Resto.
#
# #for each county polygon, find the proportion representation of each PA type
# #parallelize for speed
library(foreach)
library(doMC)

registerDoMC(cores = 4)
index = 1:nrow(counties)
outMatrix = foreach(i=index) %dopar% {
  countyPoly = gBuffer(counties[index == i,], byid = TRUE, width = 0) #fixes topology error (RGEOS)
  z = over(countyPoly, protectedAreas, returnList=TRUE)[[1]]
  if(nrow(z)>0){
    z$IUCN_CAT = factor(z$IUCN_CAT, paLevels)
    z$area = NA
    countyArea = areaPolygon(countyPoly) * 1e-6
    for(k in 1:nrow(z)){
      paPoly = protectedAreas[protectedAreas$NAME %in% z$NAME[k],]
      z$area[k] = rgeos::gIntersection(countyPoly, paPoly) %>% 
        areaPolygon * 1e-6
    }
    with(z, tapply(area, list(IUCN_CAT), sum)) / countyArea
  } else{
    rep(NA, 7)
  }
  
} %>% 
  do.call("rbind",.) %>% 
  as_tibble
outMatrix[is.na(outMatrix)] = 0
pa_by_county = data.frame(outMatrix)
names(pa_by_county) = paste0("PCT_PA_AREA_", paLevels)
pa_by_county$TOT_PA_AREA = rowSums(pa_by_county)

vars = names(pa_by_county)[-which(colSums(pa_by_county)==0)]
for(v in 1:length(vars)){
  d[vars[v]] <- pa_by_county[[vars[v]]]
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
  filter(!duplicated(name))
#
##get variables
voteShare = cbind(countyDemo$trump16,countyDemo$clinton16)
countyDemo$trump16_pct = (voteShare / rowSums(voteShare))[,1]

vars = c("total_population","white_pct","black_pct","hispanic_pct","foreignborn_pct", 
         "trump16_pct","lesscollege_pct","clf_unemploy_pct","lesshs_pct",
         "age29andunder_pct","age65andolder_pct","median_hh_inc","rural_pct")#,"ruralurban_cc")
vars2 = c("total_population", "median_hh_inc","rural_pct","age29andunder_pct","age65andolder_pct")
varNames2 = c("Population","Median_Income","Pct_Rural","Pct_Under_29","Pct_Older_65")

vals = countyDemo[,vars]
vals2 = apply(log(vals+1),2,scale)
ii = complete.cases(countyDemo[,vars])


pc = princomp(vals2[ii,], cor = TRUE)
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


demog_scores = matrix(NA, nrow(countyDemo), ncol=3)
demog_scores[ii,] <- pc$scores[,1:3]
demog_scores <- data.frame(demog_scores)
names(demog_scores) = c("DEMO_PC1","DEMO_PC2","DEMO_PC3")
demog_scores$name = countyDemo$name

d <-
  d %>% 
  left_join(demog_scores, by="name")

countyDemo[,vars2] = round(countyDemo[,vars2],2)
newVals = countyDemo[,c("name",vars2)]
names(newVals) = c("name",varNames2)
d2 <-
  d2 %>% 
  left_join(newVals, by="name")

## Save data table to storage
saveRDS(d, "county_data.rds")
d2$state = d2$state %>% str_replace_all("_", " ") %>% str_to_title()
d2$county = d2$county %>% str_replace_all("_", " ") %>% str_to_title()
d2$Mean_Temp_F = ((d2$Mean_Temp_F-32) * (5/9)) %>% round(2)
saveRDS(d2[,-c(1:3)], "summary_data.rds")

