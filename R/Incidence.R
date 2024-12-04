#' @title Incidence Module Class
#'
#' @include IncidencePrevalence.R
#'
#' @description
#' Incidence module that overrides the plotting method.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' ipInstalled <- require(
#'   "IncidencePrevalence",
#'   character.only = TRUE,
#'   quietly = TRUE,
#'   warn.conflicts = FALSE
#' )
#'
#' if (ipInstalled) {
#'
#'   inc <- readRDS(system.file(
#'     package = "DarwinShinyModules",
#'     "dummyData/IncidencePrevalence/rds/incidence.rds"
#'   ))
#'
#'   incidence <- Incidence$new(data = inc)
#' }
#'
#' if (interactive()) {
#'     preview(incidence)
#' }
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
