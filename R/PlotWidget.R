#' @title PlotWidget
#'
#' @description
#' Widget Module
#'
#' @inherit Plot
#'
#' @export
PlotWidget <- R6::R6Class(
  classname = "PlotWidget",
  inherit = Plot,

  # Public ----
  public = list(
    #' UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Widget") {
      shiny::tagList(
        shiny::h3(title),
        shiny::uiOutput(shiny::NS(private$.appId, private$id("plot")))
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
      output[[private$id("plot")]] <- shiny::renderUI({
        self$updateData(private$.data)
        do.call(what = private$.fun, args = list(data = private$.reactiveValues$data))
      })
    }
  )
)
