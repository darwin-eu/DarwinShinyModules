#' @title Plot Decorator Class
#'
#' @include ShinyModule.R
#'
#' @description
#' This class is a `decorator` and is not meant to be directly used, but to be
#' inherited by other modules, like `PlotStaic`, `PlotWidget`, and `PlotPlotly`.
#'
#' @details
#' The inherited `Plot` modules evaluate the provided function with a provided
#' data object.
#'
#' To add a new plot type it is required to inherit from the `Plot` class, and
#' to override the private `.UI()` and `.server()` methods.
#'
#' @export
Plot <- R6::R6Class(
  classname = "Plot",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field title (`character(1)`) Title to use for the plot.
    title = function(title) {
      if (missing(title)) {
        return(private$.title)
      } else {
        checkmate::assertCharacter(title, len = 1)
        private$.title <- title
      }
      return(invisible(self))
    },

    #' @field fun Plotting function.
    fun = function(fun) {
      if (missing(fun)) {
        return(private$.fun)
      } else {
        checkmate::assertFunction(fun)
        private$.fun <- fun
      }
    },

    #' @field args (`reactiveValues`) Arguments used for plot.
    args = function(args) {
      if (missing(args)) {
        return(private$.args)
      } else {
        private$.args <- args
      }
    },

    #' @field plot Plot object.
    plot = function() {
      return(private$.plot)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param fun (`function()`) Function to plot with, with one argument: `data`.
    #' @param title (`character(1)`) Title of the plot. When set to `NULL`, no title is shown.
    #'
    #' @return `self`
    initialize = function(fun, args, title = "Plot") {
      super$initialize()
      private$.data <- data
      private$.fun <- fun
      private$.args <- args
      private$.title <- title
      self$validate()
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::reportAssertions(assertions)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .fun = NULL,
    .args = NULL,
    .title = "",
    .data = NULL,
    .plot = NULL,

    ## Methods ----
    .server = function(input, output, session) {
      if (is.list(private$.args)) {
        private$.args <- do.call(shiny::reactiveValues, private$.args)
      }
    }
  )
)
