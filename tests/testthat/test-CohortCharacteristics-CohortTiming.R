test_that("Creation: CohortTiming", {
  skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && require("DrugUtilisation", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    name = "my_cohort",
    ingredient = c("acetaminophen", "morphine", "warfarin")
  )

  result <- summariseCohortTiming(cdm$my_cohort)

  mod <- suppressWarnings(moduleCohortTiming(result = result, .softValidation = TRUE))

  expect_identical(class(mod), c("CohortTiming", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(mod$namespace))
  expect_false(is.null(mod$moduleId))
  expect_false(is.null(mod$instanceId))
  expect_identical(mod$moduleName, "CohortTiming")

  # UI
  expect_s3_class(mod$UI(), "shiny.tag.list")

  # Server
  testServer(app = mod$server, {
    expect_true(is.character(session$token))
  })
})
