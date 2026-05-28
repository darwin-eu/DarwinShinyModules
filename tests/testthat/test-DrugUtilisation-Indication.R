test_that("DrugUtilisation: Indication", {
  skip_if_not_installed(
    c("DrugUtilisation", "PatientProfiles", "CDMConnector")
  )

  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")

  cdm <- DrugUtilisation::generateIngredientCohortSet(
    cdm = cdm,
    name = "dus_cohort",
    ingredient = "acetaminophen",
    gapEra = 7
  )

  indications <- list(headache = 378253, influenza = 4266367)

  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = indications,
    name = "indications_cohort"
  )

  result <- cdm$dus_cohort |>
    PatientProfiles::addAge(
      ageGroup = list(
        `0-17` = c(0, 17),
        `>=18` = c(18, Inf)
      )
    ) |>
    PatientProfiles::addSex() |>
    DrugUtilisation::summariseIndication(
      indicationCohortName = "indications_cohort",
      unknownIndicationTable = "condition_occurrence",
      indicationWindow = list(c(-30, 0)),
      strata = list(
        "age_group",
        "sex"
      )
    )

  mod <- suppressWarnings(moduleIndication(result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
