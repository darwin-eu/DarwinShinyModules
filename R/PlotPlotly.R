#' @title PlotPlotly
#'
#' @include Plot.R
#'
#' @description
#' Plotly Module
#'
#' @field plot (`plotly`) object.
#' @field source (`character`) Source label for the plotly plot.
#' @field plotlyBindings (`reactivevalues`) bindings from the plotly object.
#'
#' @export
PlotPlotly <- R6::R6Class(
  classname = "PlotPlotly",
  inherit = Plot,

  # Private ----
  public = list(
    ## Methods ----
    #' initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param fun Function to plot with, with one argument: `data`.
    #'
    #' @return `self`
    initialize = function(appId, data, fun) {
      super$initialize(appId, data, fun)
      private$.plot <- do.call(what = private$.fun, args = list(data = private$.data))
      private$.plot$x$source <- private$id("plot")
      private$.source <- private$.plot$x$source
      return(invisible(self))
    },

    #' UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Plotly") {
      shiny::tagList(
        shiny::h3(title),
        plotly::plotlyOutput(shiny::NS(private$.appId, private$id("plot")))
      )
    },

    #' server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
      plotly::event_register(p = private$.plot, event = "plotly_selected")

      output[[private$id("plot")]] <- plotly::renderPlotly({
        self$updateData(private$.data)
        private$updateBindings()
        private$.plot
      })
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .plot = NULL,
    .source = NULL,
    .plotlyBindings = shiny::reactiveValues(
      selected = NULL
    ),

    ## Methods ----
    updateBindings = function() {
      shiny::observeEvent(plotly::event_data("plotly_selected", source = private$.source), {
        private$.plotlyBindings$selected <- plotly::event_data("plotly_selected", source = private$.source)
      })
    }
  ),

  # Active ----
  active = list(
    plot = function() return(private$.plot),
    source = function() return(private$.source),
    plotlyBindings = function() return(private$.plotlyBindings)
  )
)
