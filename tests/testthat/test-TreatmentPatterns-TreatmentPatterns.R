test_that("Creation", {
  skip_if_not(require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  skip_if_not(require("CDMConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  if (interactive()) {
    library(TreatmentPatterns)
    library(testthat)
    source("./tests/testthat/helper-TreatmentPatterns.R")
  }

  tpr <- makeTreatmentPatternsResult()

  tpMod <- moduleTreatmentPatterns(tpr)

  expect_identical(class(tpMod), c("TreatmentPatterns", "ShinyModule", "R6"))

  # UI
  expect_s3_class(tpMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = tpMod$server, {
    expect_true(is.character(session$token))
  })
})
