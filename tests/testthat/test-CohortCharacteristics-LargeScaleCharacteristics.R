testthat::test_that("CohortCharacteristics-LargeScaleCharacteristics", {
  if (interactive()) {
    source("./R/CohortCharacteristics-LargeScaleCharacteristics.R")
    source("./tests/testthat/helper-CohortCharacteristics.R")
  }

  testthat::skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && utils::packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  result <- makeLargescaleCharacteristics()

  mod <- LargeScaleCharacteristics$new(result)

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
})
