test_that("DrugUtilisation: DrugUtilisation", {
  skip_if_not_installed(
    c("DrugUtilisation", "CDMConnector")
  )

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

  mod <- suppressWarnings(moduleDrugUtilisation(result = result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
