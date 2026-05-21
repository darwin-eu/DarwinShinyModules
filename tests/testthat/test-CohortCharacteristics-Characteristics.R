test_that("Creation: Incidence", {
  skip_if_not(
    require("CohortCharacteristics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    && packageVersion("CohortCharacteristics") >= "1.1.0"
  )

  result <- omopgenerics::importSummarisedResult(system.file(
    package = "DarwinShinyModules",
    "dummyData/CohortCharacteristics/1.1.1/characteristics.csv"
  ))
  charMod <- suppressWarnings(moduleCharacteristics(result = result, .softValidation = TRUE))

  expect_identical(class(charMod), c("Characteristics", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(charMod$namespace))
  expect_false(is.null(charMod$moduleId))
  expect_false(is.null(charMod$instanceId))
  expect_identical(charMod$moduleName, "Characteristics")

  # UI
  expect_s3_class(charMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = charMod$server, {
    expect_true(is.character(session$token))
  })
})
