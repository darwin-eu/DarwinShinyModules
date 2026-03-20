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
    dplyr::mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
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

  pathwaysMod <- DarwinShinyModules::TreatmentPathways$new(treatmentPathways = result$treatment_pathways, cdmSourceInfo = result$cdm_source_info)
  eventDurationMod <- DarwinShinyModules::EventDuration$new(summaryEventDuration = result$summary_event_duration, cdmSourceInfo = result$cdm_source_info)

  testthat::expect_r6_class(pathwaysMod, class = "TreatmentPathways")
  testthat::expect_r6_class(eventDurationMod, class = "EventDuration")

  app <- list(
    TreatmentPathways = pathwaysMod,
    SummaryEventDuration = eventDurationMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})
