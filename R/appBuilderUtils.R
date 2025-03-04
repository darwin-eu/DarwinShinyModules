assertAppStructure <- function(appStructure) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertList(appStructure, types = c("ShinyModule", "list"), add = assertions)
  checkmate::assertNamed(appStructure, type = "named", add = assertions)
  checkmate::reportAssertions(assertions)
}

statusCompleted <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("check"),
    badgeStatus = NULL,
    headerText = "Study Completed"
  )
}

statusOngoing <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("rotate-right"),
    badgeStatus = NULL,
    headerText = "Study Ongoing"
  )
}

statusStopped <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("exclamation"),
    badgeStatus = NULL,
    headerText = "Study Stopped"
  )
}

studyStatus <- function(type) {
  switch(type,
         "stop" = statusStopped(),
         "complete" = statusCompleted(),
         "ongoing" = statusOngoing())
}
