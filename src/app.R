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

# Load Data -----
inputData <- read_csv("data/data.csv")
options <- read_yaml("data/options.yaml")
runDates <- read_csv("data/runDates.csv") %>%
  pull(RunDate) %>%
  sort
dataScenarios <- read_csv("data/dataScenarios.csv")
modelScenarios <- read_csv("data/modelScenarios.csv")

# Setup to load model data by run date
dataLoaded <- rep(FALSE, length(runDates)) %>%
  setNames(runDates)

loadByDate <- function(runDate) {
  runDate <- as.character(runDate)
  if(!dataLoaded[runDate]) {
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
    
      # Input: Which jurisdictions to plot
      pickerInput(
        inputId = "jurisdiction",
        label = "Jurisdictions",
        choices = inputData$Jurisdiction %>% unique,
        selected = inputData$Jurisdiction %>% unique %>% `[`(1),
        options = list(`actions-box` = TRUE),
        multiple = FALSE),
      
      # Input: Which variable to plot
      pickerInput(
        inputId = "variable",
        label = "Variables",
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
      airDatepickerInput(
        inputId = "runDates",
        label = "Forecast Dates",
        value = tail(runDates, 1) + 1,
        clearButton = TRUE,
        todayButton = TRUE
      ),
      
      # Input: x axis range
      airDatepickerInput(
        inputId = "xlim",
        label = "Plot Range",
        value = c(min(inputData$Date), max(inputData$Date)),
        clearButton = TRUE,
        todayButton = TRUE,
        range = TRUE
      )
    ),
                                           
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      
      # Output: Histogram ----
      plotOutput("mainPlot"))
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
}

# Run app ----
shinyApp(ui, server)