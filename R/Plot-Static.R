# Copyright 2024 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
    ## Fields ----
    .plotHeight = 0,
    .plotWidth = 0,

    ## Methods ----
    .UI = function() {
      shiny::fluidPage(
        shiny::h3(private$.title),
        do.call(shiny::plotOutput, args = append(list(outputId = shiny::NS(private$.namespace, "plot")), private$.dots)),
        shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        shiny::div(
          style = "display: inline-block;",
          shiny::textInput(shiny::NS(self$namespace, "download_height"), "", 10, width = "50px")
        ),
        shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
        shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        shiny::div(
          style = "display: inline-block;",
          shiny::textInput(shiny::NS(self$namespace, "download_width"), "", 20, width = "50px")
        ),
        shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
        shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
        shiny::div(
          style = "display: inline-block; margin-right:",
          shiny::textInput(shiny::NS(self$namespace, "download_dpi"), "", 300, width = "50px")
        ),
        shiny::downloadButton(shiny::NS(self$namespace, "download_plot"), "Download plot")
      )
    },

    .server = function(input, output, session) {
      super$.server(input, output, session)
      private$.serverDownload(input, output, session)

      plotHeight <- shiny::reactive({
        session$clientData[[sprintf("output_%s-plot_height", self$namespace)]]
      })

      plotWidth <- shiny::reactive({
        session$clientData[[sprintf("output_%s-plot_width", self$namespace)]]
      })

      shiny::observe({
        output$plot <- shiny::renderPlot({
          do.call(private$.fun, self$args)
        }, res = max(c(plotHeight(), plotWidth() / 1.5)) / 8) |>
          shiny::bindCache(private$.fun, self$args, plotHeight(), plotWidth())
      })
    },

    .serverDownload = function(input, output, session) {
      output$download_plot <- downloadHandler(
        filename = function() {
          "plot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            do.call(private$.fun, self$args),
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
    }
  )
)
