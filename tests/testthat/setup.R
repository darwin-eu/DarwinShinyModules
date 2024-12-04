# Dependencies
library(ggplot2)
library(plotly)

# Suggests
suggestsInstalled <- all(

)

suggestsInstalled <- sapply(
  X = c(
  "TreatmentPatterns",
  "IncidencePrevalence",
  "networkD3",
  "htmlwidgets"
  ),
  FUN = require, character.only = TRUE,
  quietly = TRUE,
  warn.conflicts = FALSE
) |>
  all()
