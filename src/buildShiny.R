# buildShiny.R
# Builds a shiny app to publish data

library(rsyncrosim)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(yaml)
library(shiny)

# Function Definitions -----

# Function to recode values of x using a lookup table
lookup <- function(x, old, new){
  dplyr::recode(x, !!!set_names(new, old))
}

# Setup ----------
myProject <- project()
myScenario <- scenario()
transformerName <- "Data Publication: Publish to Shiny"

# Load settings and definitions from SyncroSim
settings <- datasheet(myScenario, name = "epiPublish_ShinyInputs", lookupsAsFactors = F)
resultScenarios <- scenario(myProject, results = T)
transformers <- datasheet(myProject, "core_Transformer", includeKey = T)
jurisdictions <- datasheet(myProject, "epi_Jurisdiction", includeKey = T)
variableNames <- datasheet(myProject, "epi_Variable", includeKey = T)

## Parse Settings ----
summaryFunction <- settings$Summary %>%
  switch(
    "Min/Max" = function(x, upper) if(upper) max(x) else min(x),
    "None" = function(...) NA)

options <- list(
  title = settings$Title
)

## Setup shiny folder ----

# Create main shiny folder
outputBaseFolder <- file.path(dirname(ssimEnvironment()$LibraryFilePath), settings$Output)
dir.create(outputBaseFolder)

# Copy over the shiny app
file.copy(file.path(ssimEnvironment()$PackageDirectory, "app.R"), file.path(outputBaseFolder, "app.R"))

# Create folder for data files
outputFolder <- file.path(outputBaseFolder, "data")
dir.create(outputFolder)

# Save options to data folder
write_yaml(options, file.path(outputFolder, "options.yaml"))

# Prepare data ----

# Preliminary clean up of input data
inputData <- 
  datasheet(myProject, scenario = resultScenarios$scenarioId, name = "epi_DataSummary", optional = T, lookupsAsFactors = F) %>%
    mutate(
      Parent = ParentName,
      Scenario = ScenarioName,
      Transformer = lookup(TransformerID, transformers$TransformerID, transformers$TransformerDisplayName),
      Date = as_date(Timestep),
      Variable = lookup(Variable, variableNames$VariableID, variableNames$Name),
      Jurisdiction = lookup(Jurisdiction, jurisdictions$JurisdictionID, jurisdictions$Name),
      Model = !is.na(Iteration)) %>%
    select(Parent, ScenarioID, Scenario, Transformer, Date, Iteration, Variable, Jurisdiction, Value, Model, AgeMin, AgeMax, Sex)

# Find the dates each scenario was run ---
scenarioIDs <- inputData$ScenarioID %>% unique
runDates <- rep(today(), length(scenarioIDs))

# Check there is data
if(nrow(inputData) == 0)
  stop("No input data found, please check scenario dependencies!")

# Summarize data for plotting
outputData <- inputData %>%
  # Add in run date, remove scenario ID
  mutate(RunDate = lookup(ScenarioID, scenarioIDs, runDates)) %>%
  select(-ScenarioID) %>%
  # Sum over sexes and age ranges
  group_by(Parent, Scenario, Transformer, Jurisdiction, Variable, Date, RunDate, Model, Iteration) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  # Summarize over iterations
  group_by(Parent, Scenario, Transformer, Jurisdiction, Variable, Date, RunDate, Model)  %>%
  summarise(
    Upper = summaryFunction(Value, upper = T),
    Lower = summaryFunction(Value, upper = F),
    Value = mean(Value)) %>%
  ungroup() %>%
  # Create compound variable for colouring data by result scenario and transformer
  # - Since scenarios can have multiple tranformers in their pipeline
  mutate(DataSeries = str_c(Scenario, " - ", Transformer))
  
# Save non-modeled data
outputData %>%
  filter(!Model) %>%
  write_csv(file.path(outputFolder, "data.csv"))

# Save modeled data by date
outputData %>%
  filter(Model) %>%
  group_by(RunDate) %>%
  group_split %>%
  walk(~ write_csv(.x, file.path(outputFolder, str_c(.x$RunDate[1], ".csv"))))

# Save list of Run Dates
outputData %>%
  select(RunDate) %>%
  unique %>%
  write_csv(file.path(outputFolder, "runDates.csv"))

# Save list of Data scenarios and transformers
outputData %>%
  filter(!Model) %>%
  select(Parent, Scenario, Transformer, DataSeries) %>%
  unique %>%
  write_csv(file.path(outputFolder, "dataScenarios.csv"))

# Save list of Model scenarios and transformers
outputData %>%
  filter(Model) %>%
  select(Parent, Scenario, Transformer, DataSeries) %>%
  unique %>%
  write_csv(file.path(outputFolder, "modelScenarios.csv"))
  
# Run the App ----
runApp(file.path(outputBaseFolder, "app.R"), launch.browser = T)