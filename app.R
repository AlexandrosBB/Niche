## Source R functions in current working directory
src_files <- list.files("R/", full.names = TRUE)
sapply(src_files, function(x)
  source(x))

## Load R Packages
#Shiny-related
library(shiny)
library(dashboardthemes)
library(shinythemes)
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
library(plotly)
#Misc
library(tidyr)
library(stringr)
library(tidyverse)
library(dplyr)

## Variable transformation function
transform_data <- function(x) {
  min_x = min(x, na.rm = TRUE)
  max_x = max(x, na.rm = TRUE)
  mean_x = mean(x, na.rm = TRUE)
  y = (x - mean_x) / (max_x - min_x)
  return(y)
}

## Set page variables
p4s_ll <- CRS("+proj=longlat")
d <- readRDS("data/county_data.rds")
counties <- as(d, "Spatial")
proj4string(counties) <- p4s_ll
centroids <-
  sapply(d$geometry, function(x)
    colMeans(x[[1]][[1]])) %>% t %>% data.frame()
d$long = centroids[, 1]
d$lat = centroids[, 2]
d2 <- readRDS("data/summary_data.rds")
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
label_county = HTML('<p style="color:black;margin-left:0px;text-align:center">County</p>')
label_state = HTML('<p style="color:black;margin-left:0px;text-align:center">State</p>')

## CSS Customization
css <- HTML(" body {
    background-color:#F2F2F2;
    margin-right:5%;
    margin-left:10%;
}")


## Create Python virtual environment
reticulate::virtualenv_create(envname = "python_environment", python = "python3")
reticulate::virtualenv_install(envname = "python_environment", packages = "geopy", ignore_installed = TRUE)
reticulate::use_virtualenv("python_environment", required = TRUE)
reticulate::source_python("Python/city_coords.py")


## Create body of dashboard page
ui <-
  fluidPage(
    title = "Niche: Data-Driven Relocations",
    tags$head(tags$style(css)),
    theme = shinytheme("cosmo"),
    fluidRow(column(12,
                    align = "center",
                    h1(
                      HTML("<u>Niche: An Interface for Exploring Relocation Options</u>")
                    ),
                    br(),
                    h4(
                      HTML(
                        "<p style='font-color:#f2f2f2;font-size:25px'>Whether you are looking for familiar surroundings or want to experience something new, Niche can help you smartly explore your relocation options. The program aggregates numerous sources of county-level data covering everything from the climate, land development, politics, cost of living, and demographics. Niche considers how important each of these variables are to you when making its recommendations. Try it out! You may be surprised where a perfect new home awaits.</p>"
                      )
                    ))),
    br(),
    fluidRow(
      column(
        3,
        align = "center",
        h5(
          HTML(
            "<p style='color:black;margin-left:0px;text-align:center;font-size:18px;'>Start by answering some questions about what how important each of the following are to you about the place you live.</p>"
          )
        ),
        h5(
          HTML(
            "<p style='color:black;margin-left:0px;text-align:center;font-size:15px'>(1 - not important, 3 - neutral, 5 - very important)</p>"
          )
        ),
        sliderInput(
          inputId = "feat_im_climate",
          label = HTML(
            "<p style='color:black;margin-left:0px;text-align:center'>How important is the climate?</p>"
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
            "<p style='color:black;margin-left:0px;text-align:center'>What about demographics (age structure, education, diversity, etc.)?</p>"
          ),
          min = 1,
          max = 5,
          value = 3,
          width = slider_width
        ),
        sliderInput(
          inputId = "feat_im_econ",
          label = HTML(
            "<p style='color:black;margin-left:0px;text-align:center;vertical-align:center'>The local economy (household income, urban development, poverty rate, etc.)?</p>"
          ),
          min = 1,
          max = 5,
          value = 3,
          width = slider_width
        ),
        sliderInput(
          inputId = "feat_im_political",
          label = HTML(
            "<p style='color:black;margin-left:0px;text-align:center'>A similar political atmosphere?</p>"
          ),
          min = 1,
          max = 5,
          value = 3,
          width = slider_width
        ),
        sliderInput(
          inputId = "feat_im_pa",
          label = HTML(
            "<p style='color:black;margin-left:0px;text-align:center'>Public lands and recreation?</p>"
          ),
          min = 1,
          max = 5,
          value = 3,
          width = slider_width
        )
      ),
      column(
        3,
        align = "left",
        h5(
          HTML(
            '<p style="color:black;margin-left:35px;font-size:18px;">Niche makes recommendations by comparing your county to others throughout the country.</p>'
          ),
          .noWS = "outside"
        ),
        align = "left",
        h5(
          HTML(
            '<p style="color:black;margin-left:35px;font-size:18px;">Enter the name of your town or city...</p>'
          ),
          .noWS = "outside"
        ),
        splitLayout(
          div(style = "text-align:center;"),
          cellWidths = c("5%", "60%", "0%", "15%", "0%", "15%", "0%"),
          textInput(
            "city",
            label = "",
            placeholder = "San Francisco, CA",
            value = "",
            width = "100%"
          ),
          tags$style(type = "text/css", "#city {text-align:center}"),
          actionButton(
            "search",
            label = "",
            width = "100%",
            icon = icon("binoculars")
          ),
          tags$style(
            type = "text/css",
            "#search {display: inline-block;text-align: center;margin-top:38%;margin-right:30%}"
          ),
          actionButton(
            "reset",
            label = "",
            width = "100%",
            icon = icon("undo")
          ),
          tags$style(
            type = "text/css",
            "#reset {display: inline-block;text-align: center;margin-top:38%;margin-right:30%}"
          )
        ),
        br(),
        h5(
          HTML(
            '<p style="color:black;margin-left:35px;font-size:18px;">...Or select your state and county using the menus...</p>'
          ),
          .noWS = "outside"
        ),
        br(),
        splitLayout(
          tags$head(tags$style(
            HTML(
              "
                 .shiny-split-layout > div {
                 text-align:center;
                 overflow: visible;
                 }
                 "
            )
          )),
          # cellWidths = c("10%", "40%", "0%", "40%", "10%"),
          cellWidths = c("10%","40%","40%"),
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
        tags$style(type = "text/css", "#out_of_state {margin-left:0px}"),
        checkboxInput(
          inputId = "out_of_state",
          label = HTML(
            "<p style='margin-left:25px;font-size:18px;'>Do you want to move out of state?</p>"
          )
        ),
        actionButton(
          "find",
          label = HTML("<b>Search Your Niche</b>"),
          width = "50%"
        ),
        tags$style(type = "text/css", "#find {text-align:center; margin-left:25%}"),
        br(),
        br(),
        h5(
          HTML(
            "<p style='margin-left:35px;font-size:18px;'>An interactive map of your results may take some time to load. Please be patient! It will appear to the right when ready.</p>"
          )
        ),
        h5(
          HTML(
            "<p style='margin-left:35px;font-size:18px;'>A table summarizing your results will also appear below.</p>"
          )
        )
      ),
      br(),
      column(
        6,
        align = "center",
        fluidRow(tags$head(
          tags$style(".shiny-output-error{color:blue; font-size: 17px}")
        )),
        fluidRow(plotlyOutput("map", height = "550px")),
        h4(
          HTML(
            "<p style='text-align:center'>Counties with scores closer to 1 are more similar to your location.</p>"
          )
        )
      )
    ),
    br(),
    br(),
    column(
      12,
      align = "center",
      fluidRow(tags$head(
        tags$style(".shiny-output-error{color:blue; font-size: 17px}")
      )),
      fluidRow(
        h4(
          HTML(
            "<p>Click on the links under 'Find_homes' to search nearby home listings on Zillow. For more information about each area, click on the links under 'Location' to search Google.</p>"
          )
        ),
        dataTableOutput("focal_table", width = "100%"),
        br(),
        dataTableOutput("table", width = "100%"),
        h5(
          HTML(
            "<p><a href='https://github.com/ericvc/Niche'>Click here to view my source code</a></p>"
          )
        ),
        h5(
          HTML(
            "<p>Send questions, suggestions, or feedback to <a href='mailto:ericvc2@gmail.com'>ericvc2'at'gmail.com</a></p>"
          )
        )
      )
    )
  )

## Server logic
server = function(input, output, session) {
  state <- reactive({
    filter(menuOpts, state == input$state)
  })
  
  #Reset search options
  #Dynamic input menu
  observeEvent(input$reset, {
    updateTextInput(session, "city", value = "")
    updateSelectInput(session, "state", selected = "Alabama")
  })
  #Display report topic menu options based on report type
  observeEvent(state(), {
    choices <- sort(unique(state()$county))
    crds <- city_coords(input$city)
    if (input$city != "") {
      loc_data <-
        crds %>%
        matrix(., ncol = 2) %>%
        SpatialPoints(., p4s_ll) %>%
        over(., counties)
      if (is.na(loc_data$SP_ID)) {
        loc_data <- nearest_polygon(crds, counties)
      }
      county = loc_data["county"]$county %>%
        str_replace_all(pattern = "_", replace = " ") %>%
        str_to_title()
      updateSelectInput(session,
                        "county",
                        choices = choices,
                        selected = county)
    } else{
      updateSelectInput(session, "county", choices = choices)
    }
  })
  
  #Dynamic input menu
  observeEvent(input$search, {
    crds <- city_coords(input$city)
    loc_data <-
      crds %>%
      matrix(., ncol = 2) %>%
      SpatialPoints(., p4s_ll) %>%
      over(., counties)
    if (is.na(loc_data$SP_ID)) {
      loc_data <- nearest_polygon(crds, counties)
    }
    state <- loc_data["state"]$state %>%
      str_replace_all(pattern = "_", replace = " ") %>%
      str_to_title()
    if (input$city == "") {
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
    social_vars = c(
      "total_population",
      "white_pct",
      "black_pct",
      "hispanic_pct",
      "foreignborn_pct",
      "age29andunder_pct",
      "age65andolder_pct",
      "rural_pct"
    )
    economic_vars = c(
      "Median_HH_Inc",
      "clf_unemploy_pct",
      "poverty_pct",
      "lesscollege_pct",
      "lesshs_pct"
    )
    political_vars = c("trump16_pct", "romney12_pct")
    landscape_vars = paste0("LandCover_PC", 1:4)
    features = c(climate_vars,
                 social_vars,
                 economic_vars,
                 political_vars,
                 landscape_vars)
    weights = c(
      rep((input$feat_im_climate - 1) / 2, 3),
      rep((input$feat_im_soc - 1) / 2, length(social_vars)),
      rep((input$feat_im_econ - 1) / 2, length(economic_vars)),
      rep((input$feat_im_pol - 1) / 2, length(political_vars)),
      rep(1, length(landscape_vars))
    )
    
    vals <- d[, features]
    vals$geometry <- NULL
    vals <- vals * weights
    vals <- apply(vals, 2, transform_data)
    vals_t <- t(vals)
    attr(vals_t, "dimnames") <- NULL
    
    #Measure cosine distance among counties, relative to focal county
    focal_index <-
      which(d$name == focal_location) #index of focal location
    Score = cosine_proximity(vals_t, focal_index, measure = "angle")
    
    progress$set(message = 'Creating map...',
                 detail = '')
    
    state_df <- map_data("state")
    d$Score <- ((Score + 1) / 2) %>% round(2)
    title. = sprintf("Similarity to %s, %s",
                     str_to_title(input_county),
                     str_to_title(input_state)) #plot title
    d$label = paste(d$county, d$state, sep = ", ") %>%
      str_replace_all(pattern = "_", replacement = " ") %>%
      str_to_title()
    pop <-
      d$total_population %>% exp() %>% round() %>% formatC(
        x = .,
        digits = 0,
        big.mark = ",",
        format = "d"
      )
    income <-
      d$Median_HH_Inc %>% exp() %>% round() %>% formatC(
        x = .,
        digits = 0,
        big.mark = ",",
        format = "d"
      ) %>% paste0("$", .)
    d <- d %>%
      mutate(label = sprintf(
        "%s<br>Population: %s<br>Median Income: %s",
        label,
        pop,
        income
      ))#location labels (County, State)
    d. <- cbind(d$Score, d2)
    names(d.) <-
      c(c("Score", "County", "State"), names(d.)[-c(1:3)])
    saveRDS(d., "data/new_table.rds") #save to storage
    d <- d[order(d$Score, decreasing = TRUE),] #sort by SCORE DESC
    
    progress$set(message = 'Converting map to Plotly object...',
                 detail = 'This may take several moments.')
    progress$set(value = 2)
    
    
    p <- ggplot(d) +
      ggtitle(title., subtitle = "Scores close to 1 are most similar") +
      theme_void() +
      theme(
        text = element_text(family = "Helvetica"),
        plot.title = element_text(
          size = 17,
          face = "bold",
          hjust = 0.0
        ),
        plot.subtitle = element_text(size = 17, hjust = 0.0),
        panel.background = element_rect(fill = "gray99"),
        legend.position = c(0.2, 0.15),
        legend.direction = "horizontal",
        legend.key.height = unit(1.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(
          face = "bold",
          size = 14.5,
          vjust = 1
        ),
        legend.text = element_text(size = 13)
      ) +
      geom_point(mapping = aes(
        x = long,
        y = lat,
        text = label,
        label = Score
      )) +
      geom_sf(
        show.legend = TRUE,
        mapping = aes(fill = Score),
        color = "black",
        size = 0.1
      ) +
      geom_path(
        data = state_df,
        show.legend = TRUE,
        mapping = aes(x = long, y = lat, group = group),
        color = "white",
        size = 0.2
      ) +
      guides(fill = guide_colorbar()) +
      scale_fill_viridis_c(
        "Score",
        direction = 1,
        breaks = seq(0, 1, 0.25),
        labels = c("0.0", "", "0.5", "", "1.0"),
        limits = c(0, 1)
      )
    
    p2 <- ggplotly(p, tooltip = c("text", "label")) #%>%
    #partial_bundle()
    
    progress$set(value = 3)
    
    return(p2)
    
  })
  
  table <- eventReactive(input$find, {
    dx = readRDS("data/new_table.rds")
    if (input$out_of_state) {
      dx <- dx %>%
        filter(State != input$state)
    }
    d_out = dx[order(dx$Score, decreasing = TRUE),]
    orderCols <- c(
      "Score",
      "Find_homes",
      "Location",
      "Mean_Temp_F",
      "Precipitation_mm",
      "Precip_Variation",
      "Population",
      "Median_Income",
      "Pct_Under_29",
      "Pct_Older_65",
      "Pct_Rural",
      "Pct_PublicLands"
    )
    zillowQuery <-
      sprintf("%s-county,-%s",
              tolower(d_out$County),
              tolower(d_out$State))
    zillowUrl <-
      sprintf("https://www.zillow.com/homes/%s_rb/", zillowQuery)
    Find_homes <- sprintf("<a href='%s'>Find homes</a>", zillowUrl)
    googleQuery = sprintf("%s+county%s+%s", d_out$County, "%2C", d_out$State) %>%
      str_replace_all(pattern = "_", replacement = "+") %>%
      tolower()
    googleUrl <-
      sprintf("https://google.com/search?q=%s", googleQuery)
    Location <-
      sprintf("<a href='%s'>%s, %s</a>",
              googleUrl,
              d_out$County,
              d_out$State)
    d_out <- cbind(Find_homes, Location, d_out) %>%
      select(., -c(County, State)) %>%
      mutate(Population = formatC(Population,
                                  big.mark = ",",
                                  format = "d")) %>%
      mutate(Median_Income = paste0(
        "$",
        formatC(
          Median_Income,
          big.mark = ",",
          digits = 2,
          format = "f"
        )
      ))
    d_out <- d_out[, orderCols]
    return(d_out)
  })
  
  focal_table <- eventReactive(input$find, {
    focal_location = paste(input$county, input$state, sep = ",_") %>%
      str_replace_all(" ", "_") %>%
      tolower()
    dx = readRDS("data/new_table.rds")
    d_out = dx %>%
      filter(State == input$state) %>%
      filter(County == input$county)
    d_out$Score = 0.00
    zillowQuery <-
      sprintf("%s-county,-%s",
              tolower(d_out$County),
              tolower(d_out$State))
    zillowUrl <-
      sprintf("https://www.zillow.com/homes/%s_rb/", zillowQuery)
    Find_homes <- sprintf("<a href='%s'>Find homes</a>", zillowUrl)
    googleQuery = sprintf("%s+county%s+%s", d_out$County, "%2C", d_out$State) %>%
      str_replace_all(pattern = "_", replacement = "+") %>%
      tolower()
    googleUrl <-
      sprintf("https://google.com/search?q=%s", googleQuery)
    Location <-
      sprintf("<a href='%s'>%s, %s</a>",
              googleUrl,
              d_out$County,
              d_out$State)
    d_out <- cbind(Find_homes, Location, d_out) %>%
      select(., -c(County, State)) %>%
      mutate(Population = formatC(Population, big.mark = ",", format = "d")) %>%
      mutate(Median_Income = paste0(
        "$",
        formatC(
          Median_Income,
          big.mark = ",",
          digits = 2,
          format = "f"
        )
      ))
    orderCols <- c(
      "Score",
      "Find_homes",
      "Location",
      "Mean_Temp_F",
      "Precipitation_mm",
      "Precip_Variation",
      "Population",
      "Median_Income",
      "Pct_Under_29",
      "Pct_Older_65",
      "Pct_Rural",
      "Pct_PublicLands"
    )
    d_out <- d_out[, orderCols]
    return(d_out)
  })
  
  output$map <- renderPlotly(map())
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
