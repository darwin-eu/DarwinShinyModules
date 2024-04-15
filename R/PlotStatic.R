#' @title PlotStatic
#'
#' @include Plot.R
#'
#' @description
#' Static Plot (`plot()` or `ggplot2`) Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(ggplot2)
#'
#' staticFun <- function(data) {
#'   ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'     geom_point() +
#'     theme_bw()
#' }
#'
#' staticModule <- PlotStatic$new(appId = "app", data = iris, fun = staticFun)
#'
#' if (interactive()) {
#'   preview(staticModule)
#' }
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
        data <- if (is.null(private$.reactiveValues$data)) {
          private$.data
        } else {
          private$.reactiveValues$data
        }
        do.call(private$.fun, list(data = data))
      })
    }
  )
)
