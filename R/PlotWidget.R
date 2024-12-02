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

  # Public ----
  public = list(
    #' @description UI
    #'
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        shiny::uiOutput(shiny::NS(private$.namespace, "plot"))
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
      shiny::moduleServer(id = private$.moduleId, module = function(input, output, session) {
        output$plot <- shiny::renderUI({
          do.call(private$.fun, list(data = private$.reactiveValues$data))
        })
      })
    }
  )
)
