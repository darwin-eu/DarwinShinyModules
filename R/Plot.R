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
        } else if (private$isWidget()) {
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("plot")))
        }
      )
    },
    server = function(input, output, session) {
      private$setEventData()
      private$renderPlot(output)
    }
  ),
  private = list(
    .plot = NULL,
    .source = NULL,
    .reactiveValues = shiny::reactiveValues(
      hover = NULL,
      unhover = NULL,
      click = NULL,
      doubleclick = NULL,
      selected = NULL,
      selecting = NULL,
      brushed = NULL,
      brushing = NULL,
      deselect = NULL,
      relayout = NULL,
      restyle = NULL,
      legendclick = NULL,
      legenddoubleclick = NULL,
      clickannotation = NULL,
      afterplot = NULL,
      sunburstclick = NULL
    ),

    setEventData = function() {
      shiny::reactive({
        suppressWarnings({
          private$.plot <- plotly::event_register(p = private$.plot, event = "plotly_selected")
          print(plotly::event_data("plotly_selected", source = private$.source))
        })
      })
    },

    isStatic = function() {
      all(
        class(private$.plot) %in% c("gg", "ggplot") |
          is.expression(private$.plot)
      )
    },

    isWidget = function() {
      any(class(private$.plot) %in% c("htmlwidget"))
    },

    renderPlot = function(output) {
      if (private$isStatic()) {
        private$renderStatic(output)
      } else if (private$isWidget()) {
        private$renderWidget(output)
        private$.plot$x$source <- private$id("plot")
        private$.source <- private$.plot$x$source
      }
    },

    renderStatic = function(output) {
      output[[private$id("plot")]] <- shiny::renderPlot(expr = eval(private$.plot))
    },

    renderWidget = function(output) {
      output[[private$id("plot")]] <- shiny::renderUI(
        expr = {
          setEventData <- private$setEventData()
          setEventData()
          private$.plot
        }
      )
    }
  ),
  active = list(
    plot = function() return(private$.plot),
    source = function() return(private$.source),
    reactiveValues = function() return(private$.reactiveValues)

  )
)
