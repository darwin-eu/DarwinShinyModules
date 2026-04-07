makeLargescaleCharacteristics <- function() {
  tmpDir <- file.path(tempdir(), "CohortCharacteristics")
  dir.create(tmpDir, recursive = TRUE, showWarnings = FALSE)

  if (CDMConnector::requireEunomia()) {
    if (!file.exists(file.path(tmpDir, "lsc.csv"))) {
      con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
      cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

      cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm, name = "my_cohort", ingredient = c("warfarin", "acetaminophen"))

      cdm$my_cohort <- cdm$my_cohort |>
        PatientProfiles::addAge(ageGroup = list(
          `0 to 17` = c(0, 17),
          `>=18` = c(18, Inf)
        )) |>
        PatientProfiles::addSex()

      result <- CohortCharacteristics::summariseLargeScaleCharacteristics(
        cohort = cdm$my_cohort,
        eventInWindow = "condition_occurrence",
        strata = list("age_group", "sex")
      )
      omopgenerics::exportSummarisedResult(result, path = tmpDir, fileName = "lsc.csv")
    } else {
      result <- omopgenerics::importSummarisedResult(file.path(tmpDir, "lsc.csv"))
    }
  }

  return(result)
}
