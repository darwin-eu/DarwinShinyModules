library(shiny)
library(dplyr)

test_that("Creation of module", {
  skip_if_not(require("DrugExposureDiagnostics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  dedData <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/DrugExposureDiagnostics/1.1.1/ded.rds"))

  app <- DrugExposureDiagnostics$new(
    resultList = dedData
  )

  # Fields
  expect_false(is.null(app$namespace))
  expect_false(is.null(app$moduleId))
  expect_equal(app$moduleName, "DrugExposureDiagnostics")
  expect_false(is.null(app$instanceId))

  # UI
  expect_s3_class(app$UI(), "shiny.tag.list")

  # Server
  testServer(app = app$server, {
    expect_true(is.character(session$token))
  })
})
