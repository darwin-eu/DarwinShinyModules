test_that("Creation: Incidence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE) &&
                packageVersion("IncidencePrevalence") >= "1.2.0")

  inc <- omopgenerics::importSummarisedResult(system.file(
    package = "DarwinShinyModules",
    "dummyData/IncidencePrevalence/1.2.0/incidence.csv"
  ))
  incMod <- Incidence$new(data = inc)

  expect_identical(class(incMod), c("Incidence", "ShinyModule", "R6"))
  expect_identical(class(incMod$pickers), c("list"))
  lapply(names(incMod$pickers), FUN = function(pickerName) {
    expect_identical(class(incMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(incMod$data), c("tidy_incidence", "tbl_df", "tbl", "data.frame"))
  expect_identical(incMod$moduleName, "Incidence")
})
