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

  incMod <- DarwinShinyModules::Incidence$new(result = incResult)
  prevPointMod <- DarwinShinyModules::Prevalence$new(result = prevPointResult, defaults = list(sex = "both"))
  prevPeriodMod <- DarwinShinyModules::Prevalence$new(result = prevPeriodResult, defaults = list(sex = "both"))

  testthat::expect_r6_class(incMod, class = "Incidence")
  testthat::expect_r6_class(prevPointMod, class = "Prevalence")
  testthat::expect_r6_class(prevPeriodMod, class = "Prevalence")

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
