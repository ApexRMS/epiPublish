library(shiny)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(dplyr)
library(forcats)
library(purrr)
library(lubridate)
library(yaml)
library(stringr)
library(scales)
library(maps)
library(viridisLite)

# Load Data -----
inputData <- read_csv("data/data.csv")
options <- read_yaml("data/options.yaml")
runDates <- read_csv("data/runDates.csv") %>%
  pull(RunDate) %>%
  sort
dataScenarios <- read_csv("data/dataScenarios.csv")
modelScenarios <- read_csv("data/modelScenarios.csv")
regionLookup <- read_csv("data/regionLookup.csv")

# Load map data
worldMapData <- map_data("world") %>%
  mutate(
    Jurisdiction = region,
    Jurisdiction = dplyr::recode(Jurisdiction, !!!set_names(regionLookup$New, regionLookup$Old))) %>%
  select(-region, subregion)
regions <- worldMapData %>% pull(Jurisdiction) %>% unique

# Setup to load model data by run date
dataLoaded <- rep(FALSE, length(runDates)) %>%
  setNames(runDates)

loadByDate <- function(runDate) {
  runDate <- as.character(runDate)
  if(!dataLoaded[runDate] & file.exists(str_c("data/", runDate, ".csv"))) {
    inputData <<- read_csv(str_c("data/", runDate, ".csv")) %>%
      bind_rows(inputData)
    dataLoaded[runDate] <<- TRUE
  }
  invisible()
}

# Load most recent data
runDates %>%
  tail(1) %>%
  loadByDate

# UI ----

ui <- fluidPage(
  # App title ----
  titlePanel(options$title),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
    
      # Side bar elements for non-spatial plotting
      conditionalPanel(
        condition = "input.plotTab == 1",
          
        # Input: Which jurisdictions to plot
        pickerInput(
          inputId = "jurisdiction",
          label = "Jurisdiction",
          choices = inputData$Jurisdiction %>% unique,
          selected = inputData$Jurisdiction %>% unique %>% `[`(1),
          options = list(`actions-box` = TRUE),
          multiple = FALSE),
        
        # Input: Which variable to plot
        pickerInput(
          inputId = "variable",
          label = "Variable",
          choices = inputData$Variable %>% unique,
          selected = ifelse("Cases - Daily" %in% inputData$Variable, "Cases - Daily", inputData$Variable %>% unique %>% `[`(1)),
          multiple = FALSE),
       
        # Input: Data scenarios
        pickerInput(
          inputId = "dataScenario",
          label = "Data",
          choices = dataScenarios$Parent %>% unique,
          selected = dataScenarios$Parent %>% unique,
          options = list(`actions-box` = TRUE),
          multiple = TRUE),
        
        # Input: 
        pickerInput(
          inputId = "modelScenario",
          label = "Forecasts",
          choices = modelScenarios$Parent %>% unique,
          selected = modelScenarios$Parent %>% unique,
          options = list(`actions-box` = TRUE),
          multiple = TRUE),
        
        # Input: Forecast dates to plot
        dateInput(
          inputId = "runDates",
          label = "Forecast Date",
          value = tail(runDates, 1),
          min = head(runDates, 1),
          max = tail(runDates, 1)
        ),
        
        # Input: x axis range
        dateRangeInput(
          inputId = "xlim",
          label = "Plot Range",
          start = min(inputData$Date),
          end = max(inputData$Date),
          min = min(inputData$Date),
          max = max(inputData$Date)
        )
      ),
      
      # Sidebar elements for map plotting
      conditionalPanel(
        condition = "input.plotTab == 2",
          
        # Input: Which variable to plot
        pickerInput(
          inputId = "mapVariable",
          label = "Variable",
          choices = inputData$Variable %>% unique,
          selected = ifelse("Cases - Daily" %in% inputData$Variable, "Cases - Daily", inputData$Variable %>% unique %>% `[`(1)),
          multiple = FALSE),
       
        # Input: Data scenarios
        pickerInput(
          inputId = "mapDataScenario",
          label = "Data",
          choices = dataScenarios$Parent %>% unique,
          selected = dataScenarios$Parent %>% unique),
        
        # Input: 
        pickerInput(
          inputId = "mapModelScenario",
          label = "Forecast",
          choices = modelScenarios$Parent %>% unique,
          selected = modelScenarios$Parent %>% unique),
        
        # Input: Forecast dates to plot
        dateInput(
          inputId = "mapRunDates",
          label = "Forecast Date",
          value = tail(runDates, 1),
          min = head(runDates, 1),
          max = tail(runDates, 1)
        )
      ),
    ),
                                           
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      
      tabsetPanel(id = "plotTab",
                  type = "tabs",
                  tabPanel("Plot", value = 1, plotOutput("mainPlot")),
                  tabPanel("Map",
                           value = 2,
                           plotOutput("worldMap"),
                           sliderInput("mapDate",
                                       "Date:",
                                       min = inputData %>% pull(Date) %>% min,
                                       max = inputData %>% pull(Date) %>% max,
                                       value = runDates %>% tail(1),
                                       timeFormat = '%Y-%m-%d',
                                       animate = animationOptions(interval = 750, loop = TRUE),
                                       width = "98%"))))
  )
)

# Server ----

server <- function(input, output) {
  
  # General main plots
  output$mainPlot <- renderPlot({
      # Make sure data from all selected dates are loaded
      walk(as_date(intersect(runDates, input$runDates)), loadByDate)
      
      formattedData <- inputData %>%
        filter(
          Variable == input$variable,
          Jurisdiction %in% input$jurisdiction,
          (!Model & Parent %in% input$dataScenario) | (Model & Parent %in% input$modelScenario & RunDate %in% input$runDates))
      
      # Filter to the chosen x limits if they are valid
      if(length(input$xlim) == 2)
        formattedData <- formattedData %>%
          filter(Date >= input$xlim[1], Date <= input$xlim[2])
       
      if(nrow(formattedData) > 0){
        formattedData %>%
          ggplot(aes(Date, Value)) +
            geom_line(data = formattedData %>% filter(Model), aes(colour = Scenario)) +
            geom_point(data = formattedData %>% filter(!Model), aes(colour = Scenario, fill = Scenario), size = 1) +
            geom_ribbon(data = formattedData %>% filter(Model), aes(ymin = Lower, ymax = Upper, colour = Scenario, fill = Scenario), alpha = 0.2) +
            scale_y_continuous(label=comma) +
            scale_colour_viridis_d() +
            scale_fill_viridis_d() +
            labs(x = "Date", y = input$variable) +
            theme_bw() +
            theme(
              axis.text = element_text(family = "Helvetica Neue", size = 14),
              axis.title = element_text(family = "Helvetical Neue", face = "bold", size = 14),
              legend.title = element_blank(),
              legend.text = element_text(family = "Helvetica Neue", face = "plain", size = 14),
              legend.position = "bottom",
              legend.direction = "vertical")
      } else{
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No data found for the given settings.\nPlease select different filtering options.", family = "Helvetica Neue", face = "bold", size = 5) +
          theme_bw() +
          theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(family = "Helvetica Neue", face = "plain", size = 14),
            legend.position = "bottom",
            legend.direction = "vertical")
      }
    })
  output$worldMap <- renderPlot({
    # Make sure data from all selected dates are loaded
    walk(as_date(intersect(runDates, input$mapRunDates)), loadByDate)
    
    inputData %>%
      filter(
        Variable == input$mapVariable,
        Date == input$mapDate,
        if(input$mapDate <= input$mapRunDates) Parent %in% input$mapDataScenario else Parent %in% input$mapModelScenario & RunDate %in% input$mapRunDates) %>%
      select(Jurisdiction, Value) %>%
      right_join(worldMapData, by = "Jurisdiction") %>%
      arrange(order) %>%
      ggplot(aes(long, lat)) +
      geom_polygon(aes(group = group, fill = Value)) +
      scale_fill_viridis_c(trans = "log", labels = comma_format(accuracy = 1), breaks = as.integer(10^(0:9))) +
      labs(fill = str_c(ifelse(input$mapDate <= input$mapRunDates, "Historic\n", "Forecasted\n"), input$mapVariable)) +
      theme_void() +
      theme(
        legend.title = element_text(family = "Helvetica Neue", face = "bold", size = 14),
        legend.text = element_text(family = "Helvetica Neue", face = "plain", size = 14)
      )
  })
}

# Run app ----
shinyApp(ui, server)