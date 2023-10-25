dropTable(cdm, listTables(attr(cdm, "dbcon"), attr(cdm, "write_schema")))
# parameters ----
ingredient <- "simvastatin"
ageGroup <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
strata <- list("age_group", "sex", c("age_group", "sex"))

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

# cdm snapshot ----
info(logger, "CDM SNAPSHOT")
write_csv(snapshot(cdm), here(resultsFolder, "cdmSnapshot.csv"))
info(logger, "CDM SNAPSHOT DONE")

# create new user cohorts ----
info(logger, "CREATE NEW USER COHORTS")
info(logger, "get concept ids from ingredient")
conceptSet <- getDrugIngredientCodes(cdm = cdm, name = ingredient)
names(conceptSet) <- ingredient
info(logger, "instantiate new users cohort")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  conceptSet = conceptSet,
  name = "new_users_cohort",
  priorObservation = 365,
  gapEra = 30,
  priorUseWashout = 365,
  limit = "first",
  cohortDateRange = as.Date(c("2018-01-01", "2019-12-31")),
  imputeDuration = "none"
)
info(logger, "export new users cohort")
newUserCohorts <- cohortSet(cdm$new_users_cohort) %>%
  inner_join(cohortAttrition(cdm$new_users_cohort), by = "cohort_definition_id") %>%
  addCdmName(cdm) %>%
  mutate(table_name = attr(cdm$new_users_cohort, "table_name"))
write_csv(newUserCohorts, here(resultsFolder, "newUsersCohort.csv"))
info(logger, "NEW USER COHORTS CREATED")

# add sex and ageGroup to stratify latter ----
info(logger, "add age and sex")
cdm$new_users_cohort <- cdm$new_users_cohort %>%
  addSex() %>%
  addAge(ageGroup = ageGroup)

# subset the cdm ----
info(logger, "subset cdm")
cdm <- cdmSubsetCohort(cdm, "new_users_cohort")
cdm$drug_exposure <- cdm$drug_exposure %>% computeQuery()
cdm$condition_occurrence <- cdm$condition_occurrence %>% computeQuery()
cdm$observation <- cdm$observation %>% computeQuery()

# summarise characteristics ----
info(logger, "SUMMARISE CHARACTERISTICS")
# instantiate conditions cohorts
info(logger, "instantiate conditions")
cdm <- generateConceptCohortSet(
  cdm = cdm,
  conceptSet = codesFromConceptSet(path = here("ConceptSets", "Conditions"), cdm = cdm),
  name = "conditions",
  end = "event_end_date"
)
# instantiate medication cohorts
info(logger, "instantiate medications")
cdm <- generateConceptCohortSet(
  cdm = cdm,
  conceptSet = codesFromConceptSet(path = here("ConceptSets", "Medications"), cdm = cdm),
  name = "medications",
  end = "event_end_date"
)
# summarise characteristics
info(logger, "summarise characteristics")
characteristics <- summariseCharacteristics(
  cohort = cdm$new_users_cohort,
  strata = strata,
  ageGroup = ageGroup,
  tableIntersect = list("Visits in prior year" = list(
    tableName = "visit_occurrence", value = "count", window = c(-365, -1)
  )),
  cohortIntersect = list(
    "Conditions any time prior" = list(
      targetCohortTable = "conditions", value = "flag", window = c(-Inf, -1)
    ),
    "Medications prior year" = list(
      targetCohortTable = "medications", value = "flag", window = c(-365, -1)
    )
  )
)
write_csv(characteristics, here(resultsFolder, "characteristics.csv"))
info(logger, "CHARACTERISTICS SUMMARISED")

# summarise indication ----
info(logger, "SUMMARISE INDICATION")
# instantate indication cohorts
cdm <- generateConceptCohortSet(
  cdm = cdm,
  conceptSet = codesFromConceptSet(here("ConceptSets", "Indication"), cdm = cdm),
  name = "indication",
  limit = "all",
  end = "event_end_date"
)
# summarise indications
summaryIndication <- cdm$new_users_cohort %>%
  addIndication(
    indicationCohortName = "indication",
    indicationGap = c(0, 30, 180, Inf),
    unknownIndicationTable = c("observation", "condition_occurrence")
  ) %>%
  summariseIndication(strata = strata)
write_csv(summaryIndication, here(resultsFolder, "indication.csv"))
info(logger, "INDICATION SUMMARISED")

# summarise drug use ----
info(logger, "SUMMARISE DRUG USE")
info(logger, "get the ingredient concept id")
ingredientConceptId <- cdm[["concept"]] %>%
  filter(.data$concept_name == .env$ingredient) %>%
  filter(.data$concept_class_id == "Ingredient") %>%
  filter(.data$standard_concept == "S") %>%
  pull("concept_id")
info(logger, "add drug use data")
drugUse <- cdm$new_users_cohort %>%
  addDrugUse(
    cdm = cdm,
    ingredientConceptId = ingredientConceptId,
    conceptSet = conceptSet
  ) %>%
  summariseDrugUse(strata = strata)
write_csv(drugUse, here(resultsFolder, "drugUse.csv"))
info(logger, "DRUG USE SUMMARISED")

# summarise large scale characteristics ----
info(logger, "START LARGE SCALE CHARACTERISTICS")
lsc <- summariseLargeScaleCharacteristics(
  cohort = cdm$new_users_cohort,
  strata = strata,
  eventInWindow = c("condition_occurrence"),#, "ICD10 Sub-chapter"),
  episodeInWindow = c("drug_exposure")#, "ATC 3rd")
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

# drop the permanent tables created during the analysis ----
#dropTable(cdm, listTables(attr(cdm, "dbcon"), attr(cdm, "write_schema")))
