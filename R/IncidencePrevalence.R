#' @title IncidencePrevalence
#'
#' @include IncidencePrevalence.R
#'
#' @description
#' IncidencePrevalence super class. Composed of the Plot and Table modules.
#'
#' @field table (`Table`) Module.
#' @field plot (`PlotPlotly`) Module.
#'
#' @export
IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = Module,

  # Public ----
  public = list(
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #'
    #' @return `self`
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.table <- Table$new(appId = appId, data = data)
      private$.plot <- PlotPlotly$new(
        appId = appId,
        data = data,
        fun = private$plotIncidencePrevalence
      )
      return(invisible(self))
    },

    #' @description UI
    #'
    #' @param title (`character(1)`) Title to use for the plot.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Table") {
      shiny::tagList(
        private$.plot$UI(title = NULL),
        private$.table$UI(title = NULL)
      )
    },

    #' server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
      private$.plot$server(input, output, session)
      private$.table$server(input, output, session)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .table = NULL,
    .plot = NULL,

    ## Methods ----
    plotIncidencePrevalence = function(data) {}
  ),

  # Active ----
  active = list(
    table = function() return(private$.table),
    plot = function() return(private$.plot)
  )
)
