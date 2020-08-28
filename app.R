## Load functions in current working directory
src_files <- list.files("R/", full.names = TRUE)
sapply(src_files, function(x) source(x))

## Load R Packages
#Shiny-related
library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(DT)
#GIS
library(sf)
library(raster)
library(sp)
library(rgdal)
library(raster)
library(caTools)
library(maptools)
library(maps)
library(rgeos)
library(geosphere)
#Plotting
library(ggplot2)
library(ggrepel)
# library(plotly)
#Misc
library(tidyr)
library(stringr)
library(tidyverse)
library(dplyr)

## Variable transformation function
transform_data <- function(x){
  min_x = min(x, na.rm=TRUE)
  max_x = max(x, na.rm=TRUE)
  mean_x = mean(x, na.rm=TRUE)
  y = (x-mean_x)/(max_x-min_x)
  return(y)
}

## Set page variables
p4s_ll <- CRS("+proj=longlat")
counties <- rgdal::readOGR("raw_data/shape/counties.shp")
proj4string(counties) <- p4s_ll
d <- readRDS("county_data.rds")
# d$Median_HH_Inc <- transform_data(d$Median_HH_Inc)
# d$total_population <- transform_data(d$total_population)
# d$CLIM_PC1 <- transform_data(d$CLIM_PC1)
# d$CLIM_PC2 <- transform_data(d$CLIM_PC2)
# d$CLIM_PC3 <- transform_data(d$CLIM_PC3)
centroids <-
  sapply(d$geometry, function(x)
    colMeans(x[[1]][[1]])) %>% t %>% data.frame()
d$long = centroids[, 1]
d$lat = centroids[, 2]
d2 <- readRDS("summary_data.rds")
d2 <- d2 %>% 
  mutate(Population = exp(Population)) %>% 
  mutate(Population = round(Population)) %>% 
  mutate(Median_Income = exp(Median_Income)) %>%
  mutate(Median_Income = round(Median_Income))
menuOpts <- d[, c("county", "state")] %>% data.frame()
menuOpts$geometry <- NULL
menuOpts$state <-
  menuOpts$state %>%
  str_replace_all(pattern = "_", replacement = " ") %>%
  str_to_title()
menuOpts$county <-
  menuOpts$county %>%
  str_replace_all(pattern = "_", replacement = " ") %>%
  str_to_title()
slider_width <- "85%"
label_county = HTML('<p style="color:white;margin-left:45px">County</p>')
label_state = HTML('<p style="color:white;margin-left:45px">State</p>')


## Create Python virtual environment
reticulate::virtualenv_create(envname = "python_environment", python = "python3")
reticulate::virtualenv_install(envname = "python_environment", packages = c("geopy"), ignore_installed = TRUE)
reticulate::use_virtualenv("python_environment", required = TRUE)
reticulate::source_python("Python/city_coords.py")


## Create sidebar for app
sidebar <- dashboardSidebar(
  disable = FALSE,
  width = 300,
  # App title  w/ company logo in top right corner----
  # titlePanel(div(
  #   img(src = "index.jpg", height = 96.33), style = ""
  # )),
  #Text box input to define Domains for comparison
  h4(
    HTML(
      '<p style="color:white;margin-left:35px">Enter the name of a city...</p>'
    ),
    .noWS = "outside"
  ),
  # h5(
  #   HTML(
  #     "<b style='color:white;margin-left:70px'>Enter a location...</b>"
  #   ),
  # ),
  splitLayout(
    div(style="text-align:center;"),
    cellWidths = c("0%","65%", "20%"),
    textInput(
      "city",
      label = "",
      placeholder = "San Francisco, CA",
      value = "",
      width = "100%"
    ),
    # tags$style(type = "text/css", "#city {text-align:center}"),
    actionButton("search",
                 label = "",
                 width = "100%",
                 icon=icon("binoculars")
  ), 
  tags$style(type = "text/css", "#search {display: inline-block;text-align: center;margin-top:55%;margin-right:30%}")),
  h4(
    HTML(
      '<p style="color:white;margin-left:35px">...Or find a location with the menu...</p>'
    ),
    .noWS = "outside"
  ),
  splitLayout(
    tags$head(tags$style(
      HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
    )),
    cellWidths = c("0%", "50%", "50%"),
    selectInput(
      #selectize = TRUE,
      "state",
      label = label_state,
      selected = NULL,
      choices = unique(menuOpts$state)
    ),
    selectInput(
      #selectize = TRUE,
      "county",
      label = label_county,
      selected = NULL,
      choices = ""
    )
  ),
  splitLayout(
    tags$head(tags$style(
      HTML("
                 .shiny-split-layout > div {
                 overflow: visible;
                 }
                 ")
    )),
    cellWidths = c("0%", "65%", "30%"),
    checkboxInput(
      inputId = "out_of_state",
      label = HTML(
        "<p style='color:white;margin-left:5px'>Out show results from different states?</p>"
      )
    ),
    # tags$style(type = "text/css", "#out_of_state {margin-left:10px}"),
    numericInput(
      inputId = "n_labels",
      label = HTML(
        "<b style='color:white;margin-left:5px'>Labels</b>"
      ),
      min = 5,
      max = 30,
      step = 1,
      value = 10
    )
  ), 
  actionButton("find",
               label = HTML("<b>Find Your Niche</b>"),
               width = "50%"),
  tags$style(type = "text/css", "#find {text-align:center; margin-left:25%}")
  )
  #br(),

## Create body of dashboard page
body <- dashboardBody(
  #changing theme
  shinyDashboardThemes(theme = "journal"),
  style = "height:100%;margin-left:5%;margin-right:5%;margin-top:0%",
  fluidPage(
    fluidRow(
    column(3,
           h4(
             HTML(
               "<b style='color:black;margin-left:30px'>Answer below on a scale of 1-5</b>"
             )
           ),
           h5(
             HTML(
               "<p style='color:black;margin-left:10px'>1 - not important, 3 - neutral, 5 - very important</p>"
             )
           ),
           sliderInput(
             inputId = "feat_im_climate",
             label = HTML(
               "<b style='color:black;margin-left:35px'>How important is the climate?</b>"
             ),
             step = 1,
             min = 1,
             max = 5,
             value = 3,
             width = slider_width
           ),
           sliderInput(
             inputId = "feat_im_soc",
             label = HTML(
               "<p style='color:black;margin-left:40px'>What about demographics (age structure, education, diversity, etc.)?</p>"
             ),
             min = 1,
             max = 5,
             value = 3,
             width = slider_width
           ),
           sliderInput(
             inputId = "feat_im_econ",
             label = HTML(
               "<p style='color:black;margin-left:40px;vertical-align:center'>The local economy (household income, urban development, poverty rate, etc.)?</p>"
             ),
             min = 1,
             max = 5,
             value = 3,
             width = slider_width
           ),
           sliderInput(
             inputId = "feat_im_political",
             label = HTML(
               "<b style='color:black;margin-left:40px'>A similar political atmosphere?</b>"
             ),
             min = 1,
             max = 5,
             value = 3,
             width = slider_width
           ),
           sliderInput(
             inputId = "feat_im_pa",
             label = HTML(
               "<b style='color:black;margin-left:35px'>Public lands and recreation?</b>"
             ),
             min = 1,
             max = 5,
             value = 3,
             width = slider_width
           )
    ),
    br(),
    column(
      9,
      align = "center",
      fluidRow(tags$head(
        tags$style(".shiny-output-error{color:blue; font-size: 17px}")
      )),
      fluidRow(),
      fluidRow(plotOutput("map", height = "550px"))
    )),
    br(),
    br(),
    br(),
    column(
      12,
      align = "center",
      fluidRow(tags$head(
        tags$style(".shiny-output-error{color:blue; font-size: 17px}")
      )),
      fluidRow(),
      fluidRow(
        # style = "height:1000px",
        dataTableOutput("focal_table", width="98%"),
         br(),
        dataTableOutput("table", width="98%")
      )
    )
  )
)

## UI
ui = dashboardPage(
  dashboardHeader(title = "Niche: Easier Moving Decisions", titleWidth =
                    350),
  sidebar,
  body
)

## Server logic
server = function(input, output, session) {
  
  state <- reactive({
    filter(menuOpts, state == input$state)
  })
  
  # Display report topic menu options based on report type
  observeEvent(state(), {
    choices <- sort(unique(state()$county))
    if(input$city!=""){
      loc_data <-
        city_coords(input$city) %>%
        matrix(., ncol = 2) %>%
        SpatialPoints(., p4s_ll) %>%
        over(., counties)
      county = loc_data["county"]$county %>% 
        str_replace_all(pattern = "_", replace=" ") %>%
        str_to_title()
      updateSelectInput(session, "county", choices = choices, selected=county)
    } else{
      updateSelectInput(session, "county", choices = choices)
    }
  })
  
  #Dynamic input menu
  observeEvent(input$search, {
    #input$search
    print("test")
    loc_data <-
      city_coords(input$city) %>%
      matrix(., ncol = 2) %>%
      SpatialPoints(., p4s_ll) %>%
      over(., counties)
    state <- loc_data["state"]$state %>% 
      str_replace_all(pattern = "_", replace=" ") %>%
      str_to_title()
    if(input$city==""){
      state <- "Alabama"
    }
    updateSelectInput(session, "state", selected = state)
  })
  
  map <- eventReactive(input$find, {
    #Start progress update
    progress <- Progress$new(session, min = 1, max = 3)
    on.exit(progress$close())
    progress$set(message = 'Gathering data...',
                 detail = '')
    progress$set(value = 1)
    
    ## Plot the features
    input_county = input$county
    input_state = input$state
    focal_location = paste(input_county, input_state, sep = ",_") %>%
      str_replace_all(" ", "_") %>%
      tolower()
    
    climate_vars = paste0("CLIM_PC", 1:3)
    social_vars = c("total_population","white_pct","black_pct","hispanic_pct","foreignborn_pct",
                    "age29andunder_pct","age65andolder_pct","rural_pct")
    economic_vars = c("Median_HH_Inc","clf_unemploy_pct","poverty_pct","lesscollege_pct","lesshs_pct")
    political_vars = c("trump16_pct","romney12_pct")
    landscape_vars = paste0("LandCover_PC",1:4)
    features = c(
      climate_vars,
      social_vars,
      economic_vars,
      political_vars,
      landscape_vars
      #paste0("PCT_PA_AREA_", c("Ib", "II", "III", "IV", "V")),
      #"TOT_PA_AREA"
    )
    weights = c(rep((input$feat_im_climate - 1) / 2, 3),
                rep((input$feat_im_soc - 1) / 2, length(social_vars)),
                rep((input$feat_im_econ - 1) / 2, length(economic_vars)),
                rep((input$feat_im_pol - 1) / 2, length(political_vars)),
                rep(1, length(landscape_vars))
                )#,
                #rep((input$feat_im_pa - 1) / 4, 1))#5))
    
    vals <- d[, features]
    vals$geometry <- NULL
    vals <- vals * weights
    vals <- apply(vals,2,transform_data)
    # vals = apply(vals, 2, function(x) 
    #   2 * ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))) - 1) #[-1, 1] tra
    vals_t <- t(vals)
    attr(vals_t, "dimnames") <- NULL
    #vals_t = apply(vals_t, 2, function(x) 2 * ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))) - 1) #[-1, 1] transform

    #Cosine similarity
    focal_index <-
      which(d$name == focal_location) #index of focal location
    Score = cos_prox(vals_t, focal_index, measure = "proximity")
    
    progress$set(message = 'Creating map...',
                 detail = '')
    
    state_df <- map_data("state")
    d$Score <- ((Score + 1) / 2) %>% round(2)
    title. = sprintf("Similarity to %s, %s", input_county, input_state) #plot title
    d$label = paste(d$county, d$state, sep = ", ") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title() #location labels (County, State)
    d. <- cbind(d$Score, d2)
    names(d.) <-
      c(c("Score", "County", "State"), names(d.)[-c(1:3)])
    saveRDS(d., "new_table.rds") #save to storage
    d <- d[order(d$Score, decreasing = TRUE),] #sort by SCORE DESC
    if (input$out_of_state) {
      d$Score[d$state==input$state] <- NA
      dlabel = d %>%
        filter(state != tolower(input$state))
      dlabel <- dlabel[1:input$n_labels, ]
    }
    else{
      dlabel <- d[1:input$n_labels, ]
    }
    p <- ggplot() +
      ggtitle(title., subtitle = "Scores close to 1 are most similar") +
      theme_void() +
      theme(
        text = element_text(family = "Helvetica"),
        plot.title = element_text(
          size = 20,
          face = "bold",
          hjust = 0.0
        ),
        plot.subtitle = element_text(size = 17, hjust = 0.0),
        panel.background = element_rect(fill = "gray99"),
        legend.position = c(0.2, 0.15),
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(
          face = "bold",
          size = 14.5,
          vjust = 1
        ),
        legend.text = element_text(size = 13)
      ) +
      geom_sf(
        data = d,
        show.legend = TRUE,
        mapping = aes(fill = Score),
        color = "black",
        size = 0.1
      ) +
      geom_path(
        data = state_df,
        show.legend = TRUE,
        mapping = aes(x=long, y=lat, group=group),
        color = "white",
        size = 0.5
      ) +
      geom_label_repel(
        data = dlabel,
        inherit.aes = FALSE,
        mapping = aes(x = long, y = lat, label = label)
      ) +
      # guides(fill = guide_colorbar()) +
      scale_fill_viridis_c(
        "Score",
        direction = 1,
        breaks = seq(0, 1, 0.25),
        labels = c("0.0", "", "0.5", "", "1.0"),
        limits = c(0, 1)
      )
    
    progress$set(message = 'Converting map to Plotly object...',
                 detail = 'This may take several moments.')
    
    progress$set(value = 2)
    
    # p <- ggplotly(p) %>%
    #   partial_bundle()
    
    
    progress$set(value = 3)
    
    return(p)
    
  })
  
  table <- eventReactive(input$find, {
    dx = readRDS("new_table.rds")
    if (input$out_of_state) {
      dx <- dx %>%
        filter(State != input$state)
    }
    d_out = dx[order(dx$Score, decreasing = TRUE), ]
    zillowQuery <- sprintf("%s-county,-%s", tolower(d_out$County), tolower(d_out$State))
    url <- sprintf("https://www.zillow.com/homes/%s_rb/",zillowQuery)
    Find_homes <- sprintf("<a href='%s'>Find homes</a>", url)
    googleQuery = sprintf("%s+county%s+%s",input$county,"%2C", input$state) %>%
      str_replace_all(pattern = "_", replacement = "+") %>%
      tolower()
    google_url <- sprintf("https://google.com/search?q=%s",googleQuery)
    d_out$County <- sprintf("<a href='%s'>%s</a>", google_url, d_out$County)
    d_out <- cbind(Find_homes, d_out)
    return(d_out)
  })
  
  focal_table <- eventReactive(input$find, {
    focal_location = paste(input$county, input$state, sep = ",_") %>%
      str_replace_all(" ", "_") %>%
      tolower()
    dx = readRDS("new_table.rds")
    d_out = dx %>% filter(State == input$state) %>% filter(County == input$county)
    d_out$Score = 0.00
    return(d_out)
  })
  
  output$map <- renderPlot(map())
  output$table <- DT::renderDataTable({
    table()
  }, rownames = FALSE, escape = FALSE, options = list(
    scrollX = TRUE,
    scrollCollapse = TRUE,
    lengthMenu = list(c(10, 20, 50), c('10', '20', '50')),
    pageLength = 10
  ))
  output$focal_table <- DT::renderDataTable({
    focal_table()
  }, rownames = FALSE, escape = FALSE, options = list(
    bFilter = FALSE,
    dom = 't',
    scrollX = TRUE,
    scrollCollapse = TRUE
  ))
  
}

shinyApp(ui = ui, server = server)
