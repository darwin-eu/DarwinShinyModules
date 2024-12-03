ableToRun <- function() {
  list(
    treatmentPatterns = require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    incidencePrevalence = require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )
}
