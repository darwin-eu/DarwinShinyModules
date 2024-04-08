#' @title Sunburst
#'
#' @include TreatmentPatterns.R
#'
#' @description
#' TreatmentPatterns super class
#'
#' @field jsShowLegend (`character(1)`) JavaScript function to show legend on
#' render as text.
#'
#' @export
Sunburst <- R6::R6Class(
  classname = "Sunburst",
  inherit = TreatmentPatterns,

  private = list(
    .jsShowLegend = "
    function(el, x) {
      d3.select(el).select('.sunburst-togglelegend').property('checked', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    ",

    plotSunburstSankey = function(data) {
      htmlwidgets::onRender(
        TreatmentPatterns::createSunburstPlot(
          treatmentPathways = data,
          groupCombinations = FALSE,
          legend = list(w = 400),
          withD3 = TRUE
        ),
        jsCode = private$.jsShowLegend
      )
    }
  ),
  active = list(
    jsShowLegend = function() return(private$.jsShowLegend)
  )
)
