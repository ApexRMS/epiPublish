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
transformers <- datasheet(myProject, "core_Transformer", includeKey = T)
stages <- datasheet(myProject, "core_StageName", includeKey = T)
jurisdictions <- datasheet(myProject, "epi_Jurisdiction", includeKey = T)
variableNames <- datasheet(myProject, "epi_Variable", includeKey = T)

## Parse Settings ----
summaryFunction <- function(x, upper) if(upper) quantile(x, 1 - (100 - settings$Percentile) / 200) else quantile(x, (100 - settings$Percentile) / 200)

settings$Output <- settings$ShinyURL %>%
  str_split("\\/") %>%
  unlist %>%
  tail(1)

settings$ShinyUser <- settings$ShinyURL %>%
  str_split("\\/") %>%
  unlist %>%
  str_subset("\\.") %>%
  str_split("\\.") %>%
  unlist() %>%
  head(1)

options <- list(
  title = settings$Title
)

## Setup shiny folder ----

# Create main shiny folder
outputBaseFolder <- file.path(dirname(ssimEnvironment()$LibraryFilePath), settings$Output)
dir.create(outputBaseFolder)

# Copy over the shiny app
file.copy(file.path(str_replace_all(ssimEnvironment()$PackageDirectory, "\\\\", "/"), "app.R"), file.path(outputBaseFolder, "app.R"), overwrite = T)

# Create folder for data files
outputFolder <- file.path(outputBaseFolder, "data")
dir.create(outputFolder)

# Save options to data folder
write_yaml(options, file.path(outputFolder, "options.yaml"))

# Prepare data ----

# Find the scenarios to pull data from
rsyncrosim::command(list(list = NA, folders = NA, lib = ssimEnvironment()$LibraryFilePath, csv = NA)) %>%
  writeLines(file("temp.csv"))
close(file("temp.csv"))

publishFolder <- read_csv("temp.csv") %>%
  filter(`Is Lite` == "Yes") %>%
  pull(ID)
unlink("temp.csv")

if(length(publishFolder) == 0) {
  resultScenarios <- scenario(myProject, results = T)$scenarioId
} else {
  resultScenarios <-
    rsyncrosim::command(list(list = NA, library = NA, lib = ssimEnvironment()$LibraryFilePath, tree = NA, csv = NA)) %>%
    # Remove all lines before the folder of interest
    `[`((str_which(., str_c("Folder \\[", publishFolder, "\\]")) + 1):length(.)) %>%
    # Remove all lines after the folder of interest if there is another folder listed in the tree
    `[`(1:ifelse(any(str_detect(., "Folder ")), str_which(., "Folder ") - 1, length(.))) %>%
    # Keep only result scenarios
    str_subset("Result\\(S\\) ") %>%
    # Extract scenario IDs
    str_extract("\\[\\d*\\]") %>%
    str_sub(2, -2) %>%
    as.integer
}

# Identify last transformer of each scenario
finalTransformer <- datasheet(myProject, scenario = resultScenarios, name = "core_Pipeline", lookupsAsFactors = F) %>%
  group_by(ScenarioID) %>%
  filter(RunOrder == max(RunOrder)) %>%
  ungroup() %>%
  mutate(transformer = lookup(StageNameID, stages$StageNameID, stages$Name)) %>%
  pull(transformer, name = ScenarioID)

if(any(!finalTransformer %in% transformers$TransformerDisplayName))
  stop("Error: Custom stages with multiple transformers not yet implemented.")

# Preliminary clean up of input data
inputData <- 
  datasheet(myProject, scenario = resultScenarios, name = "epi_DataSummary", optional = T, lookupsAsFactors = F) %>%
    mutate(
      Parent = ParentName,
      Scenario = ScenarioName,
      Transformer = lookup(TransformerID, transformers$TransformerID, transformers$TransformerDisplayName),
      Date = as_date(Timestep),
      Variable = lookup(Variable, variableNames$VariableID, variableNames$Name),
      Jurisdiction = lookup(Jurisdiction, jurisdictions$JurisdictionID, jurisdictions$Name),
      Model = !is.na(Iteration)) %>%
    select(Parent, ScenarioID, Scenario, Transformer, Date, Iteration, Variable, Jurisdiction, Value, Model, AgeMin, AgeMax, Sex) %>%
    # Only keep data from the last transformer in the pipeline of each scenario
    filter(Transformer == finalTransformer[as.character(ScenarioID)])

# Find the dates each scenario was run ---

# The way the forecast run date is defined depends on the transformer, so we must parse that to pull the date appropriately
runDates <- finalTransformer %>%
  imap_chr(~{
    # Extract forecast run dates for models with known run dates
    if(.x == "VOC + Vaccine Model: Run Model")
      return(datasheet(myProject, scenario = as.numeric(.y), name = "epiModelVocVaccine_RunSettings")$MinimumTimestep)
    if(.x == "modelKarlenPypm_B_getExpectations")
      return(datasheet(myProject, scenario = as.numeric(.y), name = "modelKarlenPypm_ModelChoices")$EndFit)
    if(.x == "modelKarlenPypm_C_getIterations")
      return(datasheet(myProject, scenario = as.numeric(.y), name = "modelKarlenPypm_ModelChoices")$EndFit)
    
    # Otherwise, find the last data date in epi_DataSummary
      return(datasheet(myProject, scenario = as.numeric(.y), name = "epi_DataSummary")$Timestep %>% sort %>% tail(1))
    
    # # Otherwise, return the scenario run date
    # scenario(myProject, scenario = as.numeric(.y), summary = T)$lastModified %>%
    #   str_extract("[\\d\\/]*") %>%
    #   mdy %>%
    #   as.character %>%
    #   return
  }) %>%
  ymd %>%
  set_names(names(finalTransformer))

# Check there is data
if(nrow(inputData) == 0)
  stop("No input data found, please check scenario dependencies!")

# Summarize data for plotting
outputData <- inputData %>%
  # Add in run date, remove scenario ID
  mutate(RunDate = lookup(ScenarioID, names(runDates), runDates)) %>%
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
# runApp(file.path(outputBaseFolder, "app.R"), launch.browser = T)

options(rsconnect.http = "curl")
userName <- settings$ShinyUser
userToken <- read_file(settings$ShinyToken)
userSecret <- read_file(settings$ShinySecret)

rsconnect::setAccountInfo(name=userName, token=userToken, secret=userSecret)

rsconnect::deployApp(outputBaseFolder, forceUpdate = T, launch.browser = T)
