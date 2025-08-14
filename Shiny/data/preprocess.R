# shiny is prepared to work with this resultList, please do not change them
library(omopgenerics)
library(stringr)
resultList <- list(
  "summarise_omop_snapshot" = c(1L),
  "summarise_cohort_count" = c(2L, 4L, 95L),
  "summarise_cohort_attrition" = c(3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 96L),
  "prevalence" = c(23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L),
  "prevalence_attrition" = c(41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L),
  "incidence" = c(59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L),
  "incidence_attrition" = c(77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 91L, 92L, 93L, 94L),
  "summarise_characteristics" = c(97L),
  "summarise_indication" = c(98L),
  "summarise_drug_utilisation" = c(99L),
  "summarise_proportion_of_patients_covered" = c(100L),
  "summarise_treatment" = c(101L, 103L),
  "summarise_drug_restart" = c(102L)
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "data")) |>
  filterSettings(table_name != "denominator" | result_type != "summarise_cohort_attrition") 

data <- prepareResult(result, resultList)
filterValues <- defaultFilterValues(result, resultList)

save(data, filterValues, file = file.path(getwd(), "data", "shinyData.RData"))

rm(result, filterValues, resultList, data)
