test_that("DrugUtilisation: DoseCoverage", {
  skip_if_not_installed(
    c("DrugUtilisation", "CDMConnector")
  )

  cdm <- DrugUtilisation::mockDrugUtilisation()

  result <- summariseDoseCoverage(cdm = cdm, ingredientConceptId = 1125315)

  mod <- suppressWarnings(moduleDoseCoverage(result = result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
