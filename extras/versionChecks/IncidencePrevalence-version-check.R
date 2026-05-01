testthat::test_that("IncidencePrevalence", {
  CDMConnector::requireEunomia()

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

  incResult <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 180,
    completeDatabaseIntervals = TRUE
  )

  prevPointResult <- IncidencePrevalence::estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    timePoint = "start"
  )

  prevPeriodResult <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = TRUE
  )

  incMod <- DarwinShinyModules::IncidencePrevalence$new(result = incResult)
  prevPointMod <- DarwinShinyModules::IncidencePrevalence$new(result = prevPointResult)
  prevPeriodMod <- DarwinShinyModules::IncidencePrevalence$new(result = prevPeriodResult)

  testthat::expect_r6_class(incMod, class = "IncidencePrevalence")
  testthat::expect_r6_class(prevPointMod, class = "IncidencePrevalence")
  testthat::expect_r6_class(prevPeriodMod, class = "IncidencePrevalence")

  app <- list(
    Incidence = incMod,
    PointPrevalence = prevPointMod,
    PeriodPrevalence = prevPeriodMod
  )

  app <- DarwinShinyModules::launchDarwinDashboardApp(app)

  testthat::expect_identical(class(app), "shiny.appobj")

  shiny::testServer(app, {
    testthat::expect_true(is.character(session$token))
  })
})
