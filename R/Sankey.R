#' @title Sankey
#'
#' @include TreatmentPatterns.R
#'
#' @description
#' Sankey diagram Module.
#'
#' @export
Sankey <- R6::R6Class(
  classname = "Sankey",
  inherit = TreatmentPatterns,

  public = list(
    #' @field colours (`list()`) Named list of hex colour codes i.e.: `list(A = "#FF0000", B = "#00FF00")`
    colours = NULL
  ),

  private = list(
    plotSunburstSankey = function(data) {
      TreatmentPatterns::createSankeyDiagram(
        treatmentPathways = data,
        groupCombinations = private$getCombinations(),
        colors = self$colours
      )
    }
  )
)
