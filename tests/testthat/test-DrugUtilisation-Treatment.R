test_that("DrugUtilisation: Treatment", {
  skip_if_not_installed(
    c("DrugUtilisation", "CDMConnector")
  )

  cdm <- DrugUtilisation::mockDrugUtilisation()

  result <- cdm$cohort1 |>
    PatientProfiles::addAge(
      ageGroup = list(
        `0-17` = c(0, 17),
        `>=18` = c(18, Inf)
      )
    ) |>
    PatientProfiles::addSex() |>
    DrugUtilisation::summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30), c(31, 365)), strata = list("age_group", "sex")
    )

  mod <- suppressWarnings(moduleTreatment(result = result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
