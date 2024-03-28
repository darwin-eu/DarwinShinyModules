Plot <- R6::R6Class(
  classname = "Plot",
  inherit = Module,

  public = list(
    initialize = function(appId, plot) {
      super$initialize(appId)
      private$.plot <- plot
      self$validate()
    },

    validate = function() {},

    UI = function(title = "Plot") {
      shiny::tagList(
        shiny::h3(title),
        if (private$isStatic()) {
          shiny::plotOutput(outputId = shiny::NS(private$.appId, private$id("plot")))
        } else if (private$isPlotly()) {
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("plot")))
        }
      )
    },
    server = function(input, output, session) {
      private$renderPlot(output)
    }
  ),
  private = list(
    .plot = NULL,

    isStatic = function() {
      all(
        class(private$.plot) %in% c("gg", "ggplot") |
          is.expression(private$.plot)
      )
    },

    isPlotly = function() {
      all(class(private$.plot) %in% c("plotly", "htmlwidget"))
    },

    renderPlot = function(output) {
      if (private$isStatic()) {
        private$renderStatic(output)
      } else if (private$isPlotly()) {
        private$renderPlotly(output)
      }
    },

    renderStatic = function(output) {
      output[[private$id("plot")]] <- shiny::renderPlot(expr = private$.plot)
    },

    renderPlotly = function(output) {
      output[[private$id("plot")]] <- shiny::renderUI(expr = private$.plot)
    }
  ),
  active = list()
)
