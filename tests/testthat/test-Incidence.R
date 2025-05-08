test_that("Creation: Incidence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE) &&
                packageVersion("IncidencePrevalence") >= "1.2.0")

  inc <- omopgenerics::importSummarisedResult(system.file(
    package = "DarwinShinyModules",
    "dummyData/IncidencePrevalence/1.2.0/incidence.csv"
  ))
  incMod <- Incidence$new(data = inc)

  expect_identical(class(incMod), c("Incidence", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(incMod$namespace))
  expect_false(is.null(incMod$moduleId))
  expect_false(is.null(incMod$instanceId))
  expect_identical(class(incMod$pickers), c("list"))
  lapply(names(incMod$pickers), FUN = function(pickerName) {
    expect_identical(class(incMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(incMod$data), c("tidy_incidence", "tbl_df", "tbl", "data.frame"))
  expect_identical(incMod$moduleName, "Incidence")

  # UI
  expect_s3_class(incMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = incMod$server, {
    expect_true(is.character(session$token))
  })
})
