#' @title PlotPlotly
#'
#' @include Plot.R
#'
#' @description
#' Plotly Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' plotlyFun <- function(data) {
#'   plotly::ggplotly(
#'     ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'       geom_point() +
#'       theme_bw()
#'   )
#' }
#'
#' plotlyModule <- PlotPlotly$new(appId = "app", data = iris, fun = plotlyFun)
#'
#' if (interactive()) {
#'   preview(plotlyModule)
#' }
PlotPlotly <- R6::R6Class(
  classname = "PlotPlotly",
  inherit = Plot,

  # Active ----
  active = list(
    #' @field plot (`plotly`) object.
    plot = function() return(private$.plot),

    #' @field source (`character`) Source label for the plotly plot.
    source = function() return(private$.source),

    #' @field bindingds (`reactivevalues`) bindings from the plotly object.
    bindingds = function() return(private$.bindingds)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param fun Function to plot with, with one argument: `data`.
    #'
    #' @return `self`
    initialize = function(data, fun) {
      super$initialize(data, fun)
      return(invisible(self))
    },

    #' @description UI
    #'
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"))
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
      shiny::moduleServer(id = private$.moduleId, function(input, output, session) {
        output$plot <- plotly::renderPlotly({
          data <- if (is.null(private$.reactiveValues$data)) {
            private$.data
          } else {
            private$.reactiveValues$data
          }
          p <- do.call(what = private$.fun, args = list(data = data))
          plotly::event_register(p = p, event = "plotly_selected")
          private$updateBindings()
          p
        })
      })
    }
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
