#' @title Prevalence
#'
#' @include IncidencePrevalence.R
#'
#' @description
#' Prevalence Module
#'
#' @export
Prevalence <- R6::R6Class(
  classname = "Prevalence",
  inherit = IncidencePrevalence,

  # Private ----
  private = list(
    plotIncidencePrevalence = function(data) {
      plotly::ggplotly(
        IncidencePrevalence::plotPrevalence(data)
      )
    }
  )
)
