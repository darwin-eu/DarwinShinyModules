#' @title TreatmentPatterns
#'
#' @include Module.R
#'
#' @description
#' TreatmentPatterns super class
#'
#' @field widget (`PlotWidget`) Module
#' @field data Data to plot with, usually a `data.frame`-like object.
#'
#' @export
TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = Module,

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @param appId (`character(1)`)
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #'
    #' @return (`invisible(self)`)
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.data <- data
      private$.widget <- PlotWidget$new(appId, data, private$plotSunburstSankey)
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return (`tagList`)
    UI = function() {
      private$.widget$UI()
    },

    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #'
    #' @return (`NULL`
    server = function(input, output, session) {
      private$.widget$server(input, output, session)
    }
  ),

  # Private ----
  private = list(
    .widget = NULL,
    .data = NULL,

    plotSunburstSankey = function(data) {}
  ),

  active = list(
    widget = function() return(private$.widget),
    data = function() return(private$.data)
  )
)
