#' @title Plot
#'
#' @include Module.R
#'
#' @description
#' Plot Interface
#'
#' @field data Data used for plot.
#' @field fun Plotting function.
#' @field reactiveValues Reactive values.
#'
#' @export
Plot <- R6::R6Class(
  classname = "Plot",
  inherit = Module,

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

    #' @description  Update data within reactive context (`reactive()` or `observe()`)
    #'
    #' @param data Updated data
    #'
    #' @return `self`
    updateData = function(data) {
      private$.reactiveValues$data <- data
      return(invisible(self))
    }
  ),
  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .fun = NULL,
    .reactiveValues = shiny::reactiveValues(
      data = NULL,
    )
  ),

  # Active ----
  active = list(
    data = function() return(private$.data),
    fun = function() return(private$.fun),
    reactiveValues = function() return(private$.reactiveValues)
  )
)
