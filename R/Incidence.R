#' @title Incidence
#'
#' @include IncidencePrevalence.R
#'
#' @description
#' Incidence Module. Composed of the Plot and Table modules.
#'
#' @export
Incidence <- R6::R6Class(
  classname = "Incidence",
  inherit = IncidencePrevalence,

  # Private ----
  private = list(
    plotIncidencePrevalence = function(data) {
      plotly::ggplotly(
        IncidencePrevalence::plotIncidence(data)
      )
    }
  )
)
