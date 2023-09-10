renv::activate()
renv::restore()

library(DBI)
library(CDMConnector)
library(DrugUtilisation)
library(CodelistGenerator)
library(PatientProfiles)
library(dplyr)
library(here)
library(log4r)
library(readr)
library(zip)

# Connection details
db <- dbConnect("...")

# connection details
databaseAcronym <- "..."
cdmDatabaseSchema <- "..."
resultsDatabaseSchema <- "..."
resultsStem <- "dus_"

cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmDatabaseSchema),
  writeSchema = c(schema = resultsDatabaseSchema, prefix = resultsStem),
  cdmName = databaseAcronym
)

# Count number of individuals in database to see if we connected correctly
cdm$person %>%
  tally() %>%
  compute()

# run analysis
source("RunStudy.R")

# happy for the long journey
cat("Study finished\n-Please see the zip folder created with all the generated csv files")
