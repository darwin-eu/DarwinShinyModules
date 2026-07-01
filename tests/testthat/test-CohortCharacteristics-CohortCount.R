test_that("Creation: CohortCount", {
  skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  cdm <- CohortCharacteristics::mockCohortCharacteristics()

  result <- cdm$cohort1 |>
    PatientProfiles::addSex() |>
    CohortCharacteristics::summariseCohortCount(strata = "sex")

  mod <- suppressWarnings(moduleCohortCount(result = result, .softValidation = TRUE))

  expect_identical(class(mod), c("CohortCount", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(mod$namespace))
  expect_false(is.null(mod$moduleId))
  expect_false(is.null(mod$instanceId))
  expect_identical(mod$moduleName, "CohortCount")

  # UI
  expect_s3_class(mod$UI(), "shiny.tag.list")

  # Server
  testServer(app = mod$server, {
    expect_true(is.character(session$token))
  })
})
