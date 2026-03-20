testthat::test_that("CohortCharacteristics", {
  CDMConnector::requireEunomia()
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

  cdm <- DrugUtilisation::generateIngredientCohortSet(
    cdm = cdm,
    name = "my_cohort",
    ingredient = c("warfarin", "acetaminophen")
  )

  cdm$my_cohort <- cdm$my_cohort |>
    PatientProfiles::addAge(ageGroup = list(
      `0 to 17` = c(0, 17),
      `>=18` = c(18, Inf)
    )) |>
    PatientProfiles::addSex()

  lscResult <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$my_cohort,
    eventInWindow = "condition_occurrence",
    strata = list("age_group", "sex")
  )

  attritionResult <- CohortCharacteristics::summariseCohortAttrition(cdm$my_cohort)

  charResult <- CohortCharacteristics::summariseCharacteristics(
    cdm$my_cohort,
    strata = list("age_group", "sex"),
    demographics = TRUE
  )

  lscMod <- DarwinShinyModules::LargeScaleCharacteristics$new(result = lscResult)
  attritionMod <- DarwinShinyModules::CohortAttrition$new(result = attritionResult)
  charMod <- DarwinShinyModules::Characteristics$new(result = charResult)

  testthat::expect_r6_class(lscMod, class = "LargeScaleCharacteristics")
  testthat::expect_r6_class(attritionMod, class = "CohortAttrition")
  testthat::expect_r6_class(charMod, class = "Characteristics")

  app <- list(
    Attrition = attritionMod,
    Characterisation = charMod,
    LargeScaleCharacterisation = lscMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    expect_true(is.character(session$token))
  })
})
