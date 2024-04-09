#' @title Sunburst
#'
#' @include TreatmentPatterns.R
#'
#' @description
#' Sunburst Module.
#'
#' @field jsShowLegend (`character(1)`) JavaScript function to show legend on
#' render as text.
#'
#' @export
Sunburst <- R6::R6Class(
  classname = "Sunburst",
  inherit = TreatmentPatterns,

  public = list(
    #' @field colours (`list()`) Named list of names (domain) and hex colour
    #' codes (range):\cr `list(domain = c("A", "B"), range = c("#FF0000", "#00FF00"))`.
    #' See \link[sunburstR]{sunburst}
    colours = NULL
  ),

  private = list(
    .jsShowLegend = "
    function(el, x) {
      d3.select(el).select('.sunburst-togglelegend').property('checkeda', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    ",

    plotSunburstSankey = function(data) {
      htmlwidgets::onRender(
        TreatmentPatterns::createSunburstPlot(
          treatmentPathways = data,
          groupCombinations = private$getCombinations(),
          legend = list(w = 400),
          withD3 = TRUE,
          colors = self$colours
        ),
        jsCode = private$.jsShowLegend
      )
    }
  ),
  active = list(
    jsShowLegend = function() return(private$.jsShowLegend)
  )
)
