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
    #' @param title (`character(1)`) Title to use for the plot.
    title = function(title) {
      if (missing(title)) {
        return(private$.title)
      } else {
        checkmate::assertCharacter(title, len = 1)
        private$.title <- title
      }
      return(invisible(self))
    },

    #' @field data Data used for plot.
    data = function(data) {
      if (missing(data)) {
        return(isolate(private$.reactiveValues$data))
      } else {
        checkmate::assertDataFrame(data)
        private$.reactiveValues$data <- data
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
    },

    #' @field reactiveValues Reactive values.
    reactiveValues = function() return(private$.reactiveValues)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param fun Function to plot with, with one argument: `data`.
    #'
    #' @return `self`
    initialize = function(data, fun, title = "Plot") {
      super$initialize()
      private$.fun <- fun
      private$.reactiveValues$data <- data
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
    .reactiveValues = shiny::reactiveValues(
      data = NULL,
    )
  )
)
