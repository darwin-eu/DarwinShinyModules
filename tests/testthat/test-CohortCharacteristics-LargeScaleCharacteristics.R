test_that("CohortCharacteristics-LargeScaleCharacteristics", {
  if (interactive()) {
    source("./R/CohortCharacteristics-LargeScaleCharacteristics.R")
    source("./tests/testthat/helper-CohortCharacteristics.R")
  }

  testthat::skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && utils::packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

  cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm, name = "my_cohort", ingredient = c("warfarin", "acetaminophen"))

  cdm$my_cohort <- cdm$my_cohort |>
    PatientProfiles::addAge(ageGroup = list(
      `0 to 17` = c(0, 17),
      `>=18` = c(18, Inf)
    )) |>
    PatientProfiles::addSex()

  result <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$my_cohort,
    eventInWindow = "condition_occurrence",
    strata = list("age_group", "sex")
  )

  mod <- suppressWarnings(moduleLargeScaleCharacteristics(result, .softValidation = TRUE))

  testthat::expect_identical(class(mod), c("LargeScaleCharacteristics", "ShinyModule", "R6"))

  # Fields
  testthat::expect_false(is.null(mod$namespace))
  testthat::expect_false(is.null(mod$moduleId))
  testthat::expect_false(is.null(mod$instanceId))
  testthat::expect_identical(mod$moduleName, "LargeScaleCharacteristics")

  # UI
  testthat::expect_s3_class(mod$UI(), "shiny.tag.list")

  # Server
  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })

  # Re-enable when chromote actually works
  # testApp <- shiny::shinyApp(ui = mod$UI(), server = mod$server)
  # app <- shinytest2::AppDriver$new(testApp)

  # testServer(mod$server, {
  #   suppressWarnings({
  #     session$setInputs(
  #       tableCDMName = "Synthea",
  #       tableCohortName = c("acetaminophen"),
  #       tableCompareBy = "cohort_name",
  #       tableSMDReference = "acetaminophen",
  #
  #       tableTopCDMName = "Synthea",
  #       tableTopCohortName = c("acetaminophen", "warfarin"),
  #       tableTopStrata = c("age_group", "sex"),
  #       tableTopVariableLevel = "0 to 0",
  #       tableTopN = 10,
  #
  #       plotCDMName = "Synthea",
  #       plotCohortName = c("acetaminophen", "warfarin"),
  #       plotFacetX = "cohort_name",
  #       plotFacetY = c("age_group", "sex"),
  #       plotColour = NULL,
  #
  #       plotComparedColour = "cohort_name",
  #       plotComparedReference = "acetaminophen",
  #       plotComparedFacetX = NULL,
  #       plotComparedFacetY = NULL
  #     )
  #
  #     session$flushReact()
  #
  #     testthat::expect_s3_class(object = output[[sprintf("%s-table", mod$table$moduleId)]], class = "json")
  #     testthat::expect_identical(unique(mod$table$args$result$cdm_name), "Synthea")
  #     testthat::expect_identical(unique(mod$table$args$result$group_level), "acetaminophen")
  #
  #     tbl1 <- output[[sprintf("%s-FlexTable", mod$tableTop$moduleId)]]
  #     session$setInputs(tableTopVariableLevel = "-inf to -366")
  #     session$flushReact()
  #     tbl2 <- output[[sprintf("%s-FlexTable", mod$tableTop$moduleId)]]
  #     testthat::expect_true(tbl1$html != tbl2$html)
  #
  #     plot1 <- output[[sprintf("%s-plot", mod$plot$moduleId)]]
  #     session$setInputs(plotCohortName = "acetaminophen")
  #     session$flushReact()
  #     plot2 <- output[[sprintf("%s-plot", mod$plot$moduleId)]]
  #     testthat::expect_true(tbl1$html != tbl2$html)
  #
  #     plot1 <- output[[sprintf("%s-plot", mod$plot$moduleId)]]
  #     session$setInputs(plotComparedReference = "warfarin")
  #     session$flushReact()
  #     plot2 <- output[[sprintf("%s-plot", mod$plot$moduleId)]]
  #     testthat::expect_true(tbl1$html != tbl2$html)
  #   })
  # })
})
