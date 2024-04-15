#' @title Sankey
#'
#' @include TreatmentPatterns.R
#'
#' @description
#' Sankey diagram Module.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' tp <- read.csv(system.file(
#'   package = "DarwinShinyModules",
#'   "dummyData/TreatmentPatterns/csv/treatmentPathways.csv"
#' ))
#'
#' sankey <- Sankey$new(appId = "id", data = tp)
#'
#' if (interactive()) {
#'   preview(sankey)
#' }
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
