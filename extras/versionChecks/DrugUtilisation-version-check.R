testthat::test_that("CohortCharacteristics", {
  CDMConnector::requireEunomia()
  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")

  cdm <- DrugUtilisation::generateIngredientCohortSet(
    cdm = cdm,
    name = "dus_cohort",
    ingredient = "acetaminophen",
    gapEra = 7
  )

  result <- cdm$dus_cohort |>
    PatientProfiles::addAge(ageGroup = list(
      `0-17` = c(0, 17),
      `>=18` = c(18, Inf)
    )) |>
    PatientProfiles::addSex() |>
    DrugUtilisation::summariseDrugUtilisation(
      ingredientConceptId = 1125315,
      gapEra = 7,
      strata = list("age_group", "sex")
    )

  duMod <- DrugUtilisation$new(result = result)

  testthat::expect_r6_class(duMod, class = "DrugUtilisation")

  app <- list(
    DrugUtilisation = duMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})
