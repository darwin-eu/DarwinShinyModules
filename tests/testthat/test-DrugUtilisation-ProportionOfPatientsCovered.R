test_that("DrugUtilisation: ProportionOfPatientsCovered", {
  skip_if_not_installed(
    c("DrugUtilisation", "CDMConnector")
  )

  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 100)

  result <- cdm$cohort1 |>
    DrugUtilisation::summariseProportionOfPatientsCovered(followUpDays = 365)

  mod <- suppressWarnings(moduleProportionOfPatientsCovered(result = result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
