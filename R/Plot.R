#' @title Plot
#'
#' @include ShinyModule.R
#'
#' @description
#' Plot Decorator class
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

    #' @field data Reactive data used for the plot. Use `shiny::isolate()` to get the non-reactive data.
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        checkmate::assertDataFrame(data)
        private$.data <- data
      }
    },

    #' @field fun Plotting function.
    fun = function(fun) {
      if (missing(fun)) {
        return(private$.fun)
      } else {
        checkmate::assertFunction(fun)
        private$.fun <- fun
      }
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param data (`data.frame`) Data to plot with, usually a `data.frame`-like object.
    #' @param fun (`function()`) Function to plot with, with one argument: `data`.
    #' @param title (`character(1)`) Title of the plot. When set to `NULL`, no title is shown.
    #'
    #' @return `self`
    initialize = function(data, fun, title = "Plot") {
      super$initialize()
      private$.data <- data
      private$.fun <- fun
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
      checkmate::assertFunction(
        .var.name = "fun",
        x = private$.fun,
        args = "data",
        add = assertions
      )
      checkmate::reportAssertions(assertions)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .fun = NULL,
    .title = "",
    .data = NULL
  )
)
