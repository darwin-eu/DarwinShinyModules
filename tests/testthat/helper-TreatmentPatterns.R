makeTreatmentPatternsResult <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  cohortSet <- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )

  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table"
  )

  cohorts <- cohortSet |>
    # Remove 'cohort' and 'json' columns
    dplyr::select(-"cohort", -"json") |>
    dplyr::mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) |>
    dplyr::rename(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
    ) |>
    dplyr::select("cohortId", "cohortName", "type")

  outputEnv <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  TreatmentPatterns::export(outputEnv)
}
