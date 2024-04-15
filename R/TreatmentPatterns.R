#' @title TreatmentPatterns
#'
#' @include ShinyModule.R
#'
#' @description
#' [TreatmentPatterns] super class. Composed of a [PlotWidget] and [Table] modules.
#' This class is an `interface` and is not meant to be directly used, but to be
#' inherited.
#'
#' @template param_appId
#' @param data Data to plot with, usually a `data.frame`-like object.
#'
#' @export
TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
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
      self$validate()
    },

    #' @description
    #' Validation method
    #'
    #' @return (`self`)
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertNames(
        .var.name = "data",
        x = names(private$.data),
        type = "named",
        must.include = c("path", "freq", "age", "sex", "indexYear")
      )
      checkmate::assertDataFrame(
        .var.name = "data",
        x = private$.data,
        any.missing = FALSE,
        min.rows = 1
      )
      checkmate::reportAssertions(assertions)
      private$assertTPInstall()
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return (`tagList`)
    UI = function() {
      shiny::tagList(
        shiny::checkboxInput(
          inputId = shiny::NS(private$.appId, self$id("inputNone")),
          label = "Show none paths",
          value = TRUE
        ),
        shiny::checkboxInput(
          inputId = shiny::NS(private$.appId, self$id("inputGroupCombinations")),
          label = "Group Combinations",
          value = TRUE
        ),
        shiny::uiOutput(shiny::NS(private$.appId, self$id("inputSex"))),
        shiny::uiOutput(shiny::NS(private$.appId, self$id("inputAge"))),
        shiny::uiOutput(shiny::NS(private$.appId, self$id("inputYear"))),
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
    #' @return (`NULL`)
    server = function(input, output, session) {
      private$updateInputs(input)
      private$updateData(private$.data)
      promises::future_promise(private$.widget$server(input, output, session))
      promises::future_promise(private$.table$server(input, output, session))
    }
  ),

  active = list(
    #' @field data Data to plot with, usually a `data.frame`-like object.
    data = function() return(private$.data),

    #' @field widget ([PlotWidget]) Module
    widget = function() return(private$.widget),

    #' @field table ([Table]) Module
    table = function() return(private$.table),

    #' @field inputs (`reactiveValues`) environment
    inputs = function() return(private$.inputs)
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

    assertTPInstall = function() {
      tpInstalled <- requireNamespace(
        "TreatmentPatterns",
        quietly = TRUE
      )

      if (!tpInstalled) {
        installTP <- readline("TreatmentPatterns is not installed, would you like to? (y/n)")
        if (tolower(installTP) == "y") {
          install.packages("TreatmentPatterns")
        } else {
          stop("TreatmentPatterns is not installed")
        }
      }
    },

    updateInputs = function(input) {
      shiny::observeEvent(
        input[[self$id("inputNone")]], {
        private$.inputs$none <- input[[self$id("inputNone")]]
      })

      shiny::observeEvent(
        input[[self$id("inputGroupCombinations")]], {
          private$.inputs$groupCombinations <- input[[self$id("inputGroupCombinations")]]
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
  )
)
