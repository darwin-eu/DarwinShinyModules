#' @title PlotStatic Module Class
#'
#' @include Plot.R
#'
#' @description
#' Static plot Module that handles static plots like from the `base::plot()`
#' function or `ggplot2` objects.
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
#' staticModule <- PlotStatic$new(fun = staticFun, args = list(data = iris))
#'
#' if (interactive()) {
#'   preview(staticModule)
#' }
PlotStatic <- R6::R6Class(
  classname = "PlotStatic",
  inherit = Plot,

  active = list(
    #' @field plot Plot object.
    plot = function() {
      return(isolate(eval(private$.plot)))
    }
  ),

  # Private ----
  private = list(
    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        shiny::plotOutput(shiny::NS(private$.namespace, "plot"))
      )
    },

    .server = function(input, output, session) {
      super$.server(input, output, session)
      output$plot <- shiny::renderPlot({
        if (length(shiny::reactiveValuesToList(private$.args)) > 0) {
          private$.plot <- expr(do.call(private$.fun, shiny::reactiveValuesToList(private$.args)))
          return(eval(private$.plot))
        }
      })
    }
  )
)
