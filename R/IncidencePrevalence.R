#' @title IncidencePrevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' IncidencePrevalence super class. Composed of the `Plot` and `Table` modules.
#' This class is a `decorator` and is not meant to be directly used, but to be
#' inherited.
#'
#' @export
IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field data (`data.frame`) Data the `table` and `plot` fields are based on.
    data = function() return(private$.data),

    #' @field table (`Table`) Module.
    table = function() return(private$.table),

    #' @field plot (`PlotPlotly`) Module.
    plot = function() return(private$.plot)
  ),

  # Public ----
  public = list(
    #' @description initialize
    #'
    #' @param data Incidence or Prevalence data from
    #' `IncidencePrevalence::estimateIncidence()` or
    #' `IncidencePrevalence::estimatePrevalence()`.
    #'
    #' @return `self`
    initialize = function(data) {
      super$initialize()
      private$.data <- data
      private$.table <- Table$new(data = data)
      private$.plot <- PlotPlotly$new(
        data = data,
        fun = private$plotIncidencePrevalence
      )

      private$.table$parentNamespace <- private$.namespace
      private$.plot$parentNamespace <- private$.namespace

      self$validate()
      return(invisible(self))
    },


    #' @description
    #' Validation method
    #'
    #' @return (`self`)
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertClass(
        .var.name = "data",
        x = private$.data,
        classes = c("IncidencePrevalenceResult"),
        add = assertions
      )
      checkmate::reportAssertions(assertions)
      private$assertIPInstall()
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .table = NULL,
    .plot = NULL,
    .data = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        private$.plot$UI(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.plot$server(input, output, session)
      private$.table$server(input, output, session)
      shiny::observe(private$updateData())
    },

    updateData = function() {
      private$.plot$data <- private$.data %>%
        dplyr::filter(
          dplyr::row_number() %in% private$.table$bindings$rows_all
        )
    },

    assertIPInstall = function() {
      ipInstalled <- requireNamespace(
        "IncidencePrevalence",
        quietly = TRUE
      )

      if (!ipInstalled) {
        installIP <- readline("IncidencePrevalence is not installed, would you like to? (y/n)")
        if (tolower(installIP) == "y") {
          install.packages("IncidencePrevalence")
        } else {
          stop("IncidencePrevalence is not installed")
        }
      }
    },

    ## Methods ----
    plotIncidencePrevalence = function(data) {},

    validateData = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertClass(x = data, classes = c("IncidencePrevalenceResult"))
    }
  )
)
