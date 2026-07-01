test_that("Creation: CohortOverlap", {
  skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  cdm <- mockCohortCharacteristics()
  result <- summariseCohortOverlap(cdm$cohort2)

  mod <- suppressWarnings(moduleCohortOverlap(result = result, .softValidation = TRUE))

  expect_identical(class(mod), c("CohortOverlap", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(mod$namespace))
  expect_false(is.null(mod$moduleId))
  expect_false(is.null(mod$instanceId))
  expect_identical(mod$moduleName, "CohortOverlap")

  # UI
  expect_s3_class(mod$UI(), "shiny.tag.list")

  # Server
  testServer(app = mod$server, {
    expect_true(is.character(session$token))
  })
})
