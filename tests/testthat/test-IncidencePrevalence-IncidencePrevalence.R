test_that("Incidence", {
  skip_if_not(
    require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )

  cdm <- IncidencePrevalence::mockIncidencePrevalence(
    sampleSize = 10000,
    outPre = 0.3,
    minOutcomeDays = 365,
    maxOutcomeDays = 3650
  )

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2008-01-01", "2018-01-01")),
    ageGroup = list(
      c(0, 64),
      c(65, 100)
    ),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )

  result <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("overall", "years", "months", "quarters", "weeks"),
    repeatedEvents = TRUE,
    outcomeWashout = 180,
    completeDatabaseIntervals = TRUE
  )

  incMod <- IncidencePrevalence$new(result = result, defaults = list(sex = "Both"))

  expect_identical(class(incMod), c("IncidencePrevalence", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(incMod$namespace))
  expect_false(is.null(incMod$moduleId))
  expect_false(is.null(incMod$instanceId))
  expect_identical(class(incMod$pickers), c("list"))
  lapply(names(incMod$pickers), FUN = function(pickerName) {
    expect_identical(class(incMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(incMod$result), c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(incMod$moduleName, "IncidencePrevalence")

  # UI
  expect_s3_class(incMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = incMod$server, {
    expect_true(is.character(session$token))
  })
})

test_that("Point Prevalence", {
  skip_if_not(
    require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )

  cdm <- IncidencePrevalence::mockIncidencePrevalence(
    sampleSize = 10000,
    outPre = 0.3,
    minOutcomeDays = 365,
    maxOutcomeDays = 3650
  )

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2008-01-01", "2018-01-01")),
    ageGroup = list(
      c(0, 64),
      c(65, 100)
    ),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )

  result <- IncidencePrevalence::estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "months", "quarters", "weeks")
  )

  prevMod <- IncidencePrevalence$new(result = result)

  expect_identical(class(prevMod), c("IncidencePrevalence", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(prevMod$namespace))
  expect_false(is.null(prevMod$moduleId))
  expect_false(is.null(prevMod$instanceId))
  expect_identical(class(prevMod$pickers), c("list"))
  lapply(names(prevMod$pickers), FUN = function(pickerName) {
    expect_identical(class(prevMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(prevMod$result), c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(prevMod$moduleName, "IncidencePrevalence")

  # UI
  expect_s3_class(prevMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = prevMod$server, {
    expect_true(is.character(session$token))
  })
})

test_that("Period Prevalence", {
  skip_if_not(
    require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )

  cdm <- IncidencePrevalence::mockIncidencePrevalence(
    sampleSize = 10000,
    outPre = 0.3,
    minOutcomeDays = 365,
    maxOutcomeDays = 3650
  )

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2008-01-01", "2018-01-01")),
    ageGroup = list(
      c(0, 64),
      c(65, 100)
    ),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )

  result <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "months", "quarters", "weeks")
  )

  prevMod <- IncidencePrevalence$new(result = result)

  expect_identical(class(prevMod), c("IncidencePrevalence", "ShinyModule", "R6"))

  # Fields
  expect_false(is.null(prevMod$namespace))
  expect_false(is.null(prevMod$moduleId))
  expect_false(is.null(prevMod$instanceId))
  expect_identical(class(prevMod$pickers), c("list"))
  lapply(names(prevMod$pickers), FUN = function(pickerName) {
    expect_identical(class(prevMod$pickers[[pickerName]]), c("InputPanel", "ShinyModule", "R6"))
  })
  expect_identical(class(prevMod$result), c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"))
  expect_identical(prevMod$moduleName, "IncidencePrevalence")

  # UI
  expect_s3_class(prevMod$UI(), "shiny.tag.list")

  # Server
  testServer(app = prevMod$server, {
    expect_true(is.character(session$token))
  })
})
