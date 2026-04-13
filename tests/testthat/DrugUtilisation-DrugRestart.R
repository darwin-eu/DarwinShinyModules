test_that("DrugUtilisation: DrugRestart", {
  skip_if_not_installed(
    c("DrugUtilisation", "PatientProfiles", "CDMConnector")
  )

  cdm <- DrugUtilisation::mockDrugUtilisation()

  conceptlist <- list(
   acetaminophen = 1125360,
   metformin = c(1503297, 1503327)
  )

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
   cdm = cdm,
   name = "switch_cohort",
   conceptSet = conceptlist
  )

  result <- cdm$cohort1 |>
   PatientProfiles::addAge(
     ageGroup = list(
       `0-17` = c(0, 17),
       `>=18` = c(18, Inf)
     )
   ) |>
   PatientProfiles::addSex() |>
   DrugUtilisation::summariseDrugRestart(
     switchCohortTable = "switch_cohort",
     strata = list("age_group", "sex")
   )

  mod <- DrugRestart$new(result)

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
