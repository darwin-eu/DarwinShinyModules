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
#' staticModule <- PlotStatic$new(data = iris, fun = staticFun)
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
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        shiny::plotOutput(shiny::NS(private$.namespace, "plot"))
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
        output$plot <- shiny::renderPlot({
          do.call(private$.fun, list(data = private$.reactiveValues$data))
        })
      })
    }
  )
)
