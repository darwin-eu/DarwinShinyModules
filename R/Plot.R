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
          plotly::plotlyOutput(outputId = shiny::NS(private$.appId, private$id("plot")))
        } else if (private$isWidget()) {
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("plot")))
        }
      )
    },
    server = function(input, output, session) {
      private$renderPlot(output)
      private$setEventData()
      return(invisible(self))
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
      private$.plot$x$source <- private$id("plot")
      private$.source <- private$.plot$x$source

      plotly::event_register(
        p = private$.plot,
        event = c(
          "plotly_hover", "plotly_unhover", "plotly_click", "plotly_doubleclick",
          "plotly_selected", "plotly_selecting", "plotly_brushed", "plotly_brushing",
          "plotly_deselect", "plotly_relayout", "plotly_restyle", "plotly_legendclick",
          "plotly_legenddoubleclick", "plotly_clickannotation", "plotly_afterplot",
          "plotly_sunburstclick"
        )
      )
      shiny::observeEvent(
        plotly::event_data("plotly_selected", source = private$.source),
        {
          #private$.reactiveValues$hover <- plotly::event_data("plotly_hover", source = private$.source)
          #private$.reactiveValues$unhover <- plotly::event_data("plotly_unhover", source = private$.source)
          #private$.reactiveValues$click <- plotly::event_data("plotly_click", source = private$.source)
          #private$.reactiveValues$doubleclick <- plotly::event_data("plotly_doubleclick", source = private$.source)
          private$.reactiveValues$selected <- plotly::event_data("plotly_selected", source = private$.source)
          #private$.reactiveValues$selecting <- plotly::event_data("plotly_selecting", source = private$.source)
          #private$.reactiveValues$brushed <- plotly::event_data("plotly_brushed", source = private$.source)
          #private$.reactiveValues$brushing <- plotly::event_data("plotly_brushing", source = private$.source)
          #private$.reactiveValues$deselect <- plotly::event_data("plotly_deselect", source = private$.source)
          #private$.reactiveValues$relayout <- plotly::event_data("plotly_relayout", source = private$.source)
          #private$.reactiveValues$restyle <- plotly::event_data("plotly_restyle", source = private$.source)
          #private$.reactiveValues$legendclick <- plotly::event_data("plotly_legendclick", source = private$.source)
          #private$.reactiveValues$legenddoubleclick <- plotly::event_data("plotly_legenddoubleclick", source = private$.source)
          # private$.reactiveValues$clickannotation <- plotly::event_data("plotly_clickannotation", source = private$.source)
          # private$.reactiveValues$afterplot <- plotly::event_data("plotly_afterplot", source = private$.source)
          # private$.reactiveValues$sunburstclick <- plotly::event_data("plotly_sunburstclick", source = private$.source)

          print(plotly::event_data("plotly_selected", source = private$.source))
          # print("From rv's")
          #print(private$.reactiveValues$selected)
        }
      )
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

    isPlotly = function() {
      all(class(private$.plot) %in% c("plotly", "htmlwidget"))
    },

    renderPlot = function(output) {
      if (private$isStatic()) {
        private$renderStatic(output)
      } else if (private$isPlotly()) {
        private$renderPlotly(output)
      } else if (private$isWidget()) {
        private$renderWidget(output)
      }
    },

    renderStatic = function(output) {
      output[[private$id("plot")]] <- shiny::renderPlot(expr = eval(private$.plot))
    },

    renderWidget = function(output) {
      output[[private$id("plot")]] <- shiny::renderUI(expr = private$.plot)
    },

    renderPlotly = function(output) {
      output[[private$id("plot")]] <- plotly::renderPlotly(private$.plot)
    }
  ),
  active = list(
    plot = function() return(private$.plot),
    source = function() return(private$.source),
    reactiveValues = function() return(private$.reactiveValues)

  )
)
