test_that("CohortCharacteristics: CohortCodelist", {
  skip_if_not_installed(
    c("CohortCharacteristics", "PatientProfiles", "CDMConnector")
  )

  cdm <- CohortCharacteristics::mockCohortCharacteristics()

  result <- CohortCharacteristics::summariseCohortCodelist(cdm$cohort1)

  mod <- suppressWarnings(moduleCohortCodelist(result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
