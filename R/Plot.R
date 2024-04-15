#' @title Plot
#'
#' @include ShinyModule.R
#'
#' @description
#' Plot Interface
#'
#' @export
Plot <- R6::R6Class(
  classname = "Plot",
  inherit = ShinyModule,

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
    initialize = function(appId, data, fun) {
      super$initialize(appId)
      private$.data <- data
      private$.fun <- fun
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
      checkmate::assertFALSE(
        .var.name = "data",
        x = is.null(private$.data),
        add = assertions
      )
      checkmate::assertFunction(
        .var.name = "fun",
        x = private$.fun,
        args = "data",
        add = assertions
      )
      checkmate::reportAssertions(assertions)
    },

    #' @description  Update data within reactive context (`reactive()` or `observe()`)
    #'
    #' @param data Updated data
    #'
    #' @return `self`
    updateDataReactive = function(data) {
      private$.reactiveValues$data <- data
      return(invisible(self))
    }
  ),

  # Active ----
  active = list(
    #' @field data Data used for plot.
    data = function() return(private$.data),

    #' @field fun Plotting function.
    fun = function() return(private$.fun),

    #' @field reactiveValues Reactive values.
    reactiveValues = function() return(private$.reactiveValues)
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .fun = NULL,
    .reactiveValues = shiny::reactiveValues(
      data = NULL,
    )
  )
)
