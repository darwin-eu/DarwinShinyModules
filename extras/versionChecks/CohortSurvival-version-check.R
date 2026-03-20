testthat::test_that("CohortCharacteristics", {
  CDMConnector::requireEunomia()
  cdm <- CohortSurvival::mockMGUS2cdm()

  result <- CohortSurvival::estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "death_cohort"
  )

  survMod <- DarwinShinyModules::CohortSurvival$new(result = result)

  testthat::expect_r6_class(survMod, class = "CohortSurvival")

  app <- list(
    Survival = survMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    expect_true(is.character(session$token))
  })
})
