test_that("Creation: Incidence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  inc <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/rds/incidence.rds"))
  incMod <- IncidencePrevalence$new(data = inc)

  expect_identical(class(incMod), c("IncidencePrevalence", "ShinyModule", "R6"))
  expect_identical(class(incMod$table), c("Table", "ShinyModule", "R6"))
  expect_identical(class(incMod$gtAttrition), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(incMod$gtTable), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(incMod$plotPlotly), c("PlotPlotly", "Plot", "ShinyModule", "R6"))

  expect_identical(incMod$data, inc)
  expect_identical(incMod$dataType, "Incidence")
})

test_that("Creation: Point Prevalence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  pointPrev <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/rds/pointPrevalence.rds"))
  pointPrevMod <- IncidencePrevalence$new(data = pointPrev)

  expect_identical(class(pointPrevMod), c("IncidencePrevalence", "ShinyModule", "R6"))
  expect_identical(class(pointPrevMod$table), c("Table", "ShinyModule", "R6"))
  expect_identical(class(pointPrevMod$gtAttrition), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(pointPrevMod$gtTable), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(pointPrevMod$plotPlotly), c("PlotPlotly", "Plot", "ShinyModule", "R6"))

  expect_identical(pointPrevMod$data, pointPrev)
  expect_identical(pointPrevMod$dataType, "Point Prevalence")
})

test_that("Creation: Period Prevalence", {
  skip_if_not(require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  periodPrev <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/rds/periodPrevalence.rds"))
  periodPrevMod <- IncidencePrevalence$new(data = periodPrev)

  expect_identical(class(periodPrevMod), c("IncidencePrevalence", "ShinyModule", "R6"))
  expect_identical(class(periodPrevMod$table), c("Table", "ShinyModule", "R6"))
  expect_identical(class(periodPrevMod$gtAttrition), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(periodPrevMod$gtTable), c("GTTable", "ShinyModule", "R6"))
  expect_identical(class(periodPrevMod$plotPlotly), c("PlotPlotly", "Plot", "ShinyModule", "R6"))

  expect_identical(periodPrevMod$data, periodPrev)
  expect_identical(periodPrevMod$dataType, "Period Prevalence")
})
