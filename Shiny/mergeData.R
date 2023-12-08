library(here)
library(readr)
library(tidyr)
library(dplyr)

cdmSnapshot <- read_csv(file = here("data", "cdmSnapshot.csv"), show_col_types = FALSE)

cohortDetails <- read_csv(file = here("data", "newUsersCohort.csv"), show_col_types = FALSE) %>%
  union_all(
    read_csv(file = here("data", "prevalentUsersCohort.csv"), show_col_types = FALSE)
  )
counts <- cohortDetails %>%
  group_by(cdm_name, cohort_definition_id, cohort_name) %>%
  filter(reason_id == max(reason_id)) %>%
  ungroup() %>%
  select(cdm_name, cohort_name, number_records, number_subjects)
attrition <- cohortDetails %>%
  select(cdm_name, cohort_name, number_records, number_subjects, reason_id, reason, excluded_records, excluded_subjects) %>%
  mutate(across(everything(), as.character))
settings <- cohortDetails %>%
  filter(reason_id == 1) %>%
  select(!c("cohort_definition_id", "number_records", "number_subjects", "reason_id", "reason", "excluded_records", "excluded_subjects"))

characteristics <- read_csv(file = here("data", "characteristics.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level", "Overall")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both"))

dose <- read_csv(file = here("data", "drugUse.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level", "Overall")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  mutate(estimate = as.numeric(estimate)) %>%
  filter(!variable %in% c("number subjects", "number records")) %>%
  select(cdm_name, cohort_name, age_group, sex, variable, estimate_type, estimate)

indication <- read_csv(file = here("data", "indication.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  mutate(estimate = as.numeric(estimate)) %>%
  filter(!variable %in% c("number subjects", "number records")) %>%
  select(cdm_name, cohort_name, age_group, sex, variable, variable_level, estimate_type, estimate)

lsc <- read_csv(file = here("data", "largeScaleCharacteristics.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level", "Overall")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  rename("cohort_name" = "Cohort name") %>%
  filter(estimate_type == "percentage") %>%
  mutate(percentage = as.numeric(estimate)/100) %>%
  select(cdm_name, cohort_name, age_group, sex, table_name, type, window = variable_level, concept, concept_name = variable, percentage)

incidence <- read_csv(file = here("data", "incidence.csv"), show_col_types = FALSE)

prevalence <- read_csv(file = here("data", "prevalence.csv"), show_col_types = FALSE)

discontinuation <- read_csv(file = here("data", "treatmentDiscontinuation.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level", "Overall")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  select(cdm_name, Cohort, age_group, sex, variable = estimate_type, time = variable_level, estimate_type = variable_type, estimate)

patterns <- read_csv(file = here("data", "treatmentSummary.csv"), show_col_types = FALSE) %>%
  expandStrata(group = "group_name", level = "group_level") %>%
  expandStrata(group = "strata_name", level = "strata_level") %>%
  select(-c("group_name", "group_level", "strata_name", "strata_level", "Overall")) %>%
  mutate(age_group = coalesce(age_group, "0 to 150")) %>%
  mutate(sex = coalesce(sex, "Both")) %>%
  filter(!variable %in% c("number subjects", "number records")) %>%
  select(cdm_name, cohort_name, age_group, sex, variable, variable_level, estimate_type, estimate)

save(
  cdmSnapshot, counts, attrition, settings, characteristics, dose, indication,
  discontinuation, patterns, incidence, prevalence, lsc,
  file = here("mergedData.Rdata")
)
