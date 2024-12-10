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
    ## Fields ----
    .widget = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlHtml"), label = "html"),
        shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlPng"), label = "png"),
        shiny::uiOutput(shiny::NS(private$.namespace, "plot"))
      )
    },

    .server = function(input, output, session) {
      output$plot <- shiny::renderUI({
        if (length(shiny::reactiveValuesToList(self$args)) > 0) {
          private$.widget <- do.call(private$.fun, shiny::reactiveValuesToList(self$args))
          private$.widget
        }
      })
      private$dlHtml(output)
      private$dlPng(output)
    },

    dlHtml = function(output) {
      output$dlHtml <- shiny::downloadHandler(
        filename = private$dlHtmlFilename,
        content = private$dlHtmlContent
      )
    },

    dlHtmlFilename = function() {
      return("widget.html")
    },

    dlHtmlContent = function(file) {
      htmlwidgets::saveWidget(widget = private$.widget, file = file)
    },

    dlPng = function(output) {
      output$dlPng <- shiny::downloadHandler(
        filename = private$dlPngFilename,
        content = private$dlPngContent
      )
    },

    dlPngFilename = function() {
      return("widget.png")
    },

    dlPngContent = function(file) {
      tempDir <- file.path(tempdir(), "pngContent")
      dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
      on.exit(unlink(tempDir, recursive = TRUE))
      htmlwidgets::saveWidget(widget = private$.widget, file = file.path(tempDir, "pngToSave.html"))
      webshot2::webshot(url = file.path("file:///", tempDir, "pngToSave.html"), file = file)
    }
  )
)
