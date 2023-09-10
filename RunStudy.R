# parameters ----
parameters <- list(
  ingredient = "simvastatin"
)

# initiate project ----
# create results folder
resultsFolder <- here("Results")
if (!dir.exists(resultsFolder)) {
  dir.create(resultsFolder)
}
# create log file
logFile <- here(resultsFolder, "log.txt")
if (file.exists(logFile)) {
  unlink(logFile)
}
logger <- create.logger()
logfile(logger) <- logFile
level(logger) <- "INFO"
info(logger, "LOGGER CREATED")

# record project parameters ----
info(logger, "RECORD PARAMETERS")
for (k in seq_along(parameters)) {
  info(logger, paste0("parameter: ", names(parameters)[k], " = ", parameters[[k]]))
  assign(names(parameters)[k], parameters[[k]])
}
info(logger, "PARAMETERS RECORDED")

# create new user cohorts ----
info(logger, "CREATE NEW USER COHORTS")
info(logger, "get concept ids from ingredient")
conceptSet <- getDrugIngredientCodes(cdm = cdm, name = ingredient)
info(logger, "instantiate new users cohort")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  conceptSetList = conceptSet,
  name = "new_users_cohort",
  summariseMode = "FirstEra",
  daysPriorObservation = 365,
  gapEra = 30,
  priorUseWashout = 365,
  cohortDateRange = as.Date(c("2018-01-01", "2019-12-31")),
  imputeDuration = "eliminate"
)
info(logger, "export new users cohort")
newUserCohorts <- cohortSet(cdm$new_users_cohort) %>%
  inner_join(cohortAttrition(cdm$new_users_cohort), by = "cohort_definition_id") %>%
  addCdmName(cdm) %>%
  mutate(table_name = attr(cdm$new_users_cohort, "table_name"))
write_csv(newUserCohorts, here(resultsFolder, "newUsersCohort.csv"))
info(logger, "NEW USER COHORTS CREATED")

# summarise characteristics ----
# instantiate conditions cohorts
# instantiate medication cohorts
# summarise characteristics
# summarise indication ----
# instantate indication cohorts
# summarise indications
# summarise drug use ----
# summarise large scale characteristics ----
info(logger, "START LARGE SCALE CHARACTERISTICS")
lsc <- summariseLargeScaleCharacteristics(
  cohort = cdm$new_users_cohort,
  strata = ,
)
info(logger, "export large scale characteristics")
write_csv(lsc, here(resultsFolder, "largeScaleCharacteristics.csv"))
info(logger, "LARGE SCALE CHARACTERISTICS FINISHED")
# summarise treatment discontinuation ----

# create zip file ----
info(logger, "EXPORT RESULTS")
zip(
  zipfile = here(resultsFolder, "Results.zip"),
  files = list.files(resultsFolder),
  root = resultsFolder
)
