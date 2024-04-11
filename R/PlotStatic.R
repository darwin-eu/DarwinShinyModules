#' @title PlotStatic
#'
#' @include Plot.R
#'
#' @description
#' Static Plot (`plot()` or `ggplot2`) Module
#'
#' @export
PlotStatic <- R6::R6Class(
  classname = "PlotStatic",
  inherit = Plot,

  # Public ----
  public = list(
    ## Methods ----
    #' @description UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Plot") {
      shiny::tagList(
        shiny::h3(title),
        shiny::plotOutput(shiny::NS(private$.appId, self$id("plot")))
      )
    },

    #' @description server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
      output[[self$id("plot")]] <- shiny::renderPlot({
        do.call(private$.fun, list(data = private$.reactiveValues$data))
      })
    }
  )
)
