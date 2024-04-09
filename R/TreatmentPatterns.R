#' @title TreatmentPatterns
#'
#' @include Module.R
#'
#' @description
#' TreatmentPatterns super class. Composed of a PlotWidget and Table modules.
#' This class is an `interface` and is not meant to be directly used, but to be
#' inherited.
#'
#' @field data Data to plot with, usually a `data.frame`-like object.
#' @field widget (\link[DarwinShinyModules]{PlotWidget}) Module
#' @field table (\link[DarwinShinyModules]{Table} Module
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
      private$.data <- data %>%
        dplyr::mutate(
          sex = as.factor(.data$sex),
          age = as.factor(.data$age),
          indexYear = as.factor(.data$indexYear)
        )
      private$.widget <- PlotWidget$new(appId, private$.data, private$plotSunburstSankey)
      private$.table <- Table$new(appId, private$.data)
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return (`tagList`)
    UI = function() {
      shiny::tagList(
        shiny::checkboxInput(
          inputId = shiny::NS(private$.appId, private$id("inputNone")),
          label = "Show none paths",
          value = TRUE
        ),
        shiny::checkboxInput(
          inputId = shiny::NS(private$.appId, private$id("inputGroupCombinations")),
          label = "Group Combinations",
          value = TRUE
        ),
        shiny::uiOutput(shiny::NS(private$.appId, private$id("inputSex"))),
        shiny::uiOutput(shiny::NS(private$.appId, private$id("inputAge"))),
        shiny::uiOutput(shiny::NS(private$.appId, private$id("inputYear"))),
        private$.widget$UI(title = NULL),
        private$.table$UI(title = NULL)
      )
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
      private$updateInputs(input)
      private$updateData(private$.data)
      private$.widget$server(input, output, session)
      private$.table$server(input, output, session)
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .widget = NULL,
    .table = NULL,
    .inputs = shiny::reactiveValues(
      none = NULL,
      groupCombinations = NULL
    ),

    updateInputs = function(input) {
      shiny::observeEvent(
        input[[private$id("inputNone")]], {
        private$.inputs$none <- input[[private$id("inputNone")]]
      })

      shiny::observeEvent(
        input[[private$id("inputGroupCombinations")]], {
          private$.inputs$groupCombinations <- input[[private$id("inputGroupCombinations")]]
      })
    },

    plotSunburstSankey = function(data) {},

    updateData = function(data) {
      observeEvent(list(
        private$.inputs$none,
        private$.table$bindings$rows_all), {
          data <- data %>%
            filter(
              .data$path != private$getNone(),
              dplyr::row_number() %in% private$.table$bindings$rows_all)
        private$.widget$updateDataReactive(data)
      })
    },

    getNone = function() {
      none <- if (private$.inputs$none) {
        ""
      } else {
        "None"
      }
      return(none)
    },

    getCombinations = function() {
      if (is.null(private$.inputs$groupCombinations)) {
        return(FALSE)
      } else {
        return(private$.inputs$groupCombinations)
      }
    }
  ),

  active = list(
    data = function() return(private$.data),
    widget = function() return(private$.widget),
    table = function() return(private$.table)
  )
)
