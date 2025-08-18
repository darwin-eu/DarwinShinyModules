test_that("Creation: Prevalence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE) &&
                packageVersion("IncidencePrevalence") >= "1.2.0")

  prev <- omopgenerics::importSummarisedResult(system.file(
    package = "DarwinShinyModules",
    "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"
  ))
  prevMod <- Prevalence$new(data = prev)

  expect_identical(class(prevMod), c("Prevalence", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(prevMod$namespace))
  expect_false(is.null(prevMod$moduleId))
  expect_false(is.null(prevMod$instanceId))
  expect_identical(class(prevMod$pickers), c("list"))
  lapply(names(prevMod$pickers), FUN = function(pickerName) {
    expect_identical(class(prevMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(prevMod$data), c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(prevMod$moduleName, "Prevalence")

  # UI
  expect_s3_class(prevMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = prevMod$server, {
    expect_true(is.character(session$token))
  })
})
