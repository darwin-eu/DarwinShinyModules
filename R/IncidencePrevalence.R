#' @title IncidencePrevalence
#'
#' @include ShinyModule.R
#'
#' @description
#' IncidencePrevalence super class. Composed of the Plot and Table modules.
#' This class is an `interface` and is not meant to be directly used, but to be
#' inherited.
#'
#' @export
IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = ShinyModule,

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


    #' @description
    #' Validation method
    #'
    #' @return (`self`)
    validate = function() {
      ipInstalled <- require(
        "IncidencePrevalence",
        character.only = TRUE,
        quietly = TRUE
      )

      if (!ipInstalled) {
        installIP <- readline("IncidencePrevalence is not installed, would you like to? (y/n)")
        if (installIP) {
          install.packages("IncidencePrevalence")
        } else {
          stop("IncidencePrevalence is not installed")
        }
      }
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

  # Active ----
  active = list(
    #' @field table (`Table`) Module.
    table = function() return(private$.table),

    #' @field plot (`PlotPlotly`) Module.
    plot = function() return(private$.plot)
  ),

  # Private ----
  private = list(
    ## Fields ----
    .table = NULL,
    .plot = NULL,

    ## Methods ----
    plotIncidencePrevalence = function(data) {}
  )
)
