incidenceDummyData <- function() {
  cdm <- IncidencePrevalence::mockIncidencePrevalenceRef(
    sampleSize = 1000,
    outPre = 0.5
  )

  on.exit(CDMConnector::cdmDisconnect(cdm))

  cdm <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2012-01-01")),
    ageGroup = list(c(0, 150)),
    sex = "Both",
    daysPriorObservation = 0
  )

  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    outcomeWashout = 0,
    repeatedEvents = FALSE
  )

  return(inc)
}
