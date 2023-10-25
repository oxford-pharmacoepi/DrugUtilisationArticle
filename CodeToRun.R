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
server_dbi<-"cdm_iqvia_pharmetrics_plus_202203"
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)


# connection details
databaseAcronym <- "PHARMETRICS"
cdmDatabaseSchema <- "public"
resultsDatabaseSchema <- "results"
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
  computeQuery()

# run analysis
source("RunStudy.R")

# happy for the long journey
cat("Study finished\n-Please see the zip folder created with all the generated csv files")
