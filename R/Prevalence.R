#' @title Prevalence
#'
#' @include IncidencePrevalence.R
#'
#' @description
#' Prevalence Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' prev <- readRDS(system.file(
#'   package = "DarwinShinyModules",
#'   "dummyData/IncidencePrevalence/rds/prevalence.rds"
#' ))
#'
#' prevalence <- Prevalence$new(data = prev)
#'
#' if (interactive()) {
#'   preview(prevalence)
#' }
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
