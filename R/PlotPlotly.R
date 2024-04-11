#' @title PlotPlotly
#'
#' @include Plot.R
#'
#' @description
#' Plotly Module
#'
#' @export
PlotPlotly <- R6::R6Class(
  classname = "PlotPlotly",
  inherit = Plot,

  # Private ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param fun Function to plot with, with one argument: `data`.
    #'
    #' @return `self`
    initialize = function(appId, data, fun) {
      super$initialize(appId, data, fun)
      private$.plot <- do.call(what = private$.fun, args = list(data = private$.data))
      private$.plot$x$source <- self$id("plot")
      private$.source <- private$.plot$x$source
      return(invisible(self))
    },

    #' @description UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Plotly") {
      shiny::tagList(
        shiny::h3(title),
        plotly::plotlyOutput(shiny::NS(private$.appId, self$id("plot")))
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

      output[[self$id("plot")]] <- plotly::renderPlotly({
        private$updateBindings()
        private$.plot
      })
    }
  ),

  # Active ----
  active = list(
    #' @field plot (`plotly`) object.
    plot = function() return(private$.plot),

    #' @field source (`character`) Source label for the plotly plot.
    source = function() return(private$.source),

    #' @field bindingds (`reactivevalues`) bindings from the plotly object.
    bindingds = function() return(private$.bindingds)
  ),

  # Private ----
  private = list(
    ## Fields ----
    .plot = NULL,
    .source = NULL,
    .bindingds = shiny::reactiveValues(
      selected = NULL
    ),

    ## Methods ----
    updateBindings = function() {
      shiny::observeEvent(plotly::event_data("plotly_selected", source = private$.source), {
        private$.bindingds$selected <- plotly::event_data("plotly_selected", source = private$.source)
      })
    }
  )
)
