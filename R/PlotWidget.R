#' @title PlotWidget
#'
#' @include Plot.R
#'
#' @description
#' Widget Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(networkD3)
#' src <- c(
#'   "A", "A", "A", "A",
#'   "B", "B", "C", "C", "D"
#' )
#' target <- c(
#'   "B", "C", "D", "J",
#'   "E", "F", "G", "H", "I"
#' )
#'
#' networkData <- data.frame(src, target)
#'
#' widgetFun <- function(data) {
#'   simpleNetwork(data)
#' }
#'
#' widgetModule <- PlotWidget$new(appId = "app", data = networkData, fun = widgetFun)
#' if (interactive()) {
#'   preview(plotWidget)
#' }
PlotWidget <- R6::R6Class(
  classname = "PlotWidget",
  inherit = Plot,

  # Public ----
  public = list(
    #' @description UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Widget") {
      shiny::tagList(
        shiny::h3(title),
        shiny::uiOutput(shiny::NS(private$.appId, self$id("plot")))
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
      output[[self$id("plot")]] <- shiny::renderUI({
        data <- if (is.null(private$.reactiveValues$data)) {
          private$.data
        } else {
          private$.reactiveValues$data
        }
        do.call(what = private$.fun, args = list(data = data))
      })
    }
  )
)
