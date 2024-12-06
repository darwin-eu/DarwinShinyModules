#' @title PlotWidget Module Class
#'
#' @include Plot.R
#'
#' @description
#' Widget module that handles `htmlwidget` objects.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' nD3Installed <- require(
#' "networkD3",
#' character.only = TRUE,
#' quietly = TRUE,
#' warn.conflicts = FALSE
#' )
#'
#' if (nD3Installed) {
#'   src <- c(
#'     "A", "A", "A", "A",
#'     "B", "B", "C", "C", "D"
#'   )
#'   target <- c(
#'     "B", "C", "D", "J",
#'     "E", "F", "G", "H", "I"
#'   )
#'
#'   networkData <- data.frame(src, target)
#'
#'   widgetFun <- function(data) {
#'     simpleNetwork(data)
#'   }
#'
#'   widgetModule <- PlotWidget$new(data = networkData, fun = widgetFun)
#'
#'   if (interactive()) {
#'     preview(widgetModule)
#'   }
#' }
PlotWidget <- R6::R6Class(
  classname = "PlotWidget",
  inherit = Plot,

  # Private ----
  private = list(
    .UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        shiny::uiOutput(shiny::NS(private$.namespace, "plot"))
      )
    },

    .server = function(input, output, session) {
      output$plot <- shiny::renderUI({
        do.call(private$.fun, list(data = private$.reactiveValues$data))
      })
    }
  )
)
