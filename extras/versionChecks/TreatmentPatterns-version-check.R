testthat::test_that("TreatmentPatterns", {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = file.path(tempdir(), "eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

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
    dplyr::select(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
      "type"
    )

  outputEnv <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  result <- TreatmentPatterns::export(outputEnv)

  tpMod <- DarwinShinyModules::TreatmentPatterns$new(result)

  testthat::expect_r6_class(tpMod, class = "TreatmentPatterns")

  app <- DarwinShinyModules::launchDarwinDashboardApp(
    list(
      TreatmentPatterns = tpMod
    )
  )

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})
