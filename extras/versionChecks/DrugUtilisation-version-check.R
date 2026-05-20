testthat::test_that("DrugUtilisation", {
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

testthat::test_that("Indication", {
  CDMConnector::requireEunomia()
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
    DrugUtilisation::summariseIndication(
      indicationCohortName = "indications_cohort",
      unknownIndicationTable = "condition_occurrence",
      indicationWindow = list(c(-30, 0))
    )

  indicationMod <- Indication$new(result = result)

  testthat::expect_r6_class(indicationMod, class = "Indication")

  app <- list(
    Indication = indicationMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})

testthat::test_that("Treatment", {
  CDMConnector::requireEunomia()
  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 200, source = "duckdb")

  new_cohort_set <- settings(cdm$cohort1) |>
    dplyr::arrange(cohort_definition_id) |>
    dplyr::mutate(cohort_name = c("asthma", "bronchitis", "pneumonia"))

  cdm$cohort1 <- cdm$cohort1 |>
    omopgenerics::newCohortTable(cohortSetRef = new_cohort_set)

  new_cohort_set <- settings(cdm$cohort2) |>
    dplyr::arrange(cohort_definition_id) |>
    dplyr::mutate(cohort_name = c("albuterol", "fluticasone", "montelukast"))

  cdm$cohort2 <- cdm$cohort2 |>
    omopgenerics::newCohortTable(cohortSetRef = new_cohort_set)

  result <- DrugUtilisation::summariseTreatment(
    cohort = cdm$cohort1,
    treatmentCohortName = c("cohort2"),
    window = list(c(0, 0), c(1, 30))
  )

  treatmentMod <- Treatment$new(result = result)

  testthat::expect_r6_class(treatmentMod, class = "Treatment")

  app <- list(
    Treatment = treatmentMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})

testthat::test_that("DrugRestart", {
  CDMConnector::requireEunomia()
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

  restartMod <- DrugRestart$new(result = result)

  testthat::expect_r6_class(restartMod, class = "DrugRestart")

  app <- list(
    DrugRestart = restartMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})
