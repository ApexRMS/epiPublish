library(shiny)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(dplyr)
library(forcats)
library(purrr)
library(lubridate)
library(yaml)

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

dataSerieslevels <- c(dataScenarios$DataSeries, modelScenarios$DataSeries) %>% unique %>% sort

# UI ----

ui <- fluidPage(
  # App title ----
  titlePanel(options$title),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
     
      # Input: Data scenarios
      pickerInput(
        inputId = "dataScenario",
        label = "Data Scenarios",
        choices = dataScenarios$Parent %>% unique,
        selected = dataScenarios$Parent %>% unique,
        options = list(`actions-box` = TRUE),
        multiple = TRUE),
      
      # Input: 
      pickerInput(
        inputId = "modelScenario",
        label = "Model Scenarios",
        choices = modelScenarios$Parent %>% unique,
        selected = modelScenarios$Parent %>% unique,
        options = list(`actions-box` = TRUE),
        multiple = TRUE),
      
      h3("Include:"),
      
      # Input: Dates to plot
      airDatepickerInput(
        inputId = "runDates",
        label = "Model Run Dates",
        value = tail(runDates, 1) + 1,
        range= TRUE,
        clearButton = TRUE,
        todayButton = TRUE
      ),
      
      # Input: Which variable to plot
      pickerInput(
        inputId = "variable",
        label = "Variables",
        choices = inputData$Variable %>% unique,
        selected = inputData$Variable %>% unique %>% `[`(1),
        multiple = TRUE),
    
      # Input: Which jurisdictions to plot
      pickerInput(
        inputId = "jurisdiction",
        label = "Jurisdictions",
        choices = inputData$Jurisdiction %>% unique,
        selected = inputData$Jurisdiction %>% unique,
        options = list(`actions-box` = TRUE),
        multiple = TRUE),
      
      # Input: Which transformers to plot
      pickerInput(
        inputId = "transformers",
        label = "Transformers",
        choices = c(dataScenarios$Transformer, modelScenarios$Transformer) %>% unique %>% sort,
        selected = c(dataScenarios$Transformer, modelScenarios$Transformer) %>% unique,
        options = list(`actions-box` = TRUE),
        multiple = TRUE),
      
      h3("Disaggregate by:"),
      
      # Input: Disaggregate jurisdictions
      checkboxInput(
        inputId = "jurisdictionDisaggregate",
        label = "Jurisdiction",
        value = 1
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
        Transformer %in% input$transformers,
        (!Model & Parent %in% input$dataScenario) | (Model & Parent %in% input$modelScenario & RunDate %in% input$runDates))
    
    if(!input$jurisdictionDisaggregate)
      formattedData <- formattedData %>%
        group_by(Parent, Scenario, Transformer, Variable, Date, RunDate, Model, DataSeries) %>%
        summarise(
          Value = sum(Value),
          Lower = sum(Lower),
          Upper = sum(Upper)) %>%
        ungroup %>%
        mutate(Jurisdiction = "All Jurisdictions")
    
    formattedData %>%
      ggplot(aes(Date, Value)) +
        geom_line(data = formattedData %>% filter(Model), aes(colour = DataSeries)) +
        geom_point(data = formattedData %>% filter(!Model), aes(colour = DataSeries, fill = DataSeries), size = 1) +
        geom_ribbon(data = formattedData %>% filter(Model), aes(ymin = Lower, ymax = Upper, colour = DataSeries, fill = DataSeries), alpha = 0.2) +
        facet_wrap(Jurisdiction ~ Variable, scales = "free_y") +
        theme_bw()
  })
}

# Run app ----
shinyApp(ui, server)