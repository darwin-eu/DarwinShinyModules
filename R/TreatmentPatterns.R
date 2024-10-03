#' @title TreatmentPatterns
#'
#' @include ShinyModule.R
#'
#' @description
#' [TreatmentPatterns] super class. Composed of a [PlotWidget] and [Table] modules.
#' This class is an `interface` and is not meant to be directly used, but to be
#' inherited.
#'
#' @param data Data to plot with, usually a `data.frame`-like object.
#'
#' @export
TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Active ----
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

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return (`invisible(self)`)
    initialize = function(data) {
      super$initialize()
      private$.data <- data %>%
        dplyr::mutate(
          sex = as.factor(.data$sex),
          age = as.factor(.data$age),
          indexYear = as.factor(.data$indexYear)
        )
      private$initInputPanel()
      private$initWidget()
      private$initTable()
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
        shiny::column(
          width = 12,
          shinydashboard::box(
            width = 4,
            private$.inputPanel$UI(),
            private$.table$UI()
          ),
          shinydashboard::box(
            width = 8,
            private$.widget$UI()
          )
        )
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
      shiny::moduleServer(id = private$.moduleId, function(input, output, session) {
        private$updateData(input)
        private$.inputPanel$server(input, output, session)
        private$.widget$server(input, output, session)
        private$.table$server(input, output, session)
      })
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .inputPanel = NULL,
    .widget = NULL,
    .table = NULL,
    .inputs = shiny::reactiveValues(
      none = NULL,
      groupCombinations = NULL
    ),

    ## Methods ----
    initInputPanel = function() {
      private$.inputPanel <- InputPanel$new(
        funs = list(
          none = shiny::checkboxInput,
          groupCombi = shiny::checkboxInput,
          ageGroup = shinyWidgets::pickerInput,
          sexGroup = shinyWidgets::pickerInput,
          yearGroup = shinyWidgets::pickerInput,
          freqSlider = shiny::sliderInput
        ),
        args = list(
          none = list(
            inputId = "none",
            label = "Show none paths",
            value = TRUE
          ),
          groupCombi = list(
            inputId = "groupCombi",
            label = "Group Combinations",
            value = TRUE
          ),
          ageGroup = list(
            inputId = "ageGroup",
            choices = unique(private$.data$age),
            label = "Age Group"
          ),
          sexGroup = list(
            inputId = "sexGroup",
            choices = unique(private$.data$sex),
            label = "Sex"
          ),
          yearGroup = list(
            inputId = "yearGroup",
            choices = unique(private$.data$indexYear),
            label = "Index Year"
          ),
          freqSlider = list(
            inputId = "freqSlider",
            label = "Frequency",
            min = min(private$.data$freq),
            max = max(private$.data$freq),
            value = c(min(private$.data$freq), max(private$.data$freq)),
            dragRange = TRUE,
            step = 1
          )
        )
      )
      private$.inputPanel$parentNamespace <- private$.namespace
    },

    initWidget = function() {
      private$.widget <- PlotWidget$new(data = private$.data, fun = private$plotSunburstSankey)
      private$.widget$parentNamespace <- private$.namespace
    },

    initTable = function() {
      private$.table <- Table$new(private$.data, filter = "none")
      private$.table$parentNamespace <- private$.namespace
    },

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

    plotSunburstSankey = function(data) {},

    updateData = function(input) {
      data <- shiny::eventReactive(list(
        private$.inputPanel$inputValues$none,
        private$.inputPanel$inputValues$ageGroup,
        private$.inputPanel$inputValues$sexGroup,
        private$.inputPanel$inputValues$yearGroup
      ), {
        private$.data %>%
          dplyr::filter(
            .data$path != private$getNone(),
            .data$age == private$.inputPanel$inputValues$ageGroup,
            .data$sex == private$.inputPanel$inputValues$sexGroup,
            .data$indexYear == private$.inputPanel$inputValues$yearGroup
          )
      })

      observeEvent(list(
        private$.inputPanel$inputValues$none,
        private$.inputPanel$inputValues$ageGroup,
        private$.inputPanel$inputValues$sexGroup,
        private$.inputPanel$inputValues$yearGroup
      ), {
        data <- data()
        shiny::updateSliderInput(
          inputId = shiny::NS(private$.inputPanel$moduleId, "freqSlider"),
          min = min(data$freq),
          max = max(data$freq),
          value = c(min(data$freq), max(data$freq))
        )
      })

      observeEvent(list(
        private$.inputPanel$inputValues$none,
        private$.inputPanel$inputValues$ageGroup,
        private$.inputPanel$inputValues$sexGroup,
        private$.inputPanel$inputValues$yearGroup,
        private$.inputPanel$inputValues$freqSlider
      ), {
        data <- data() %>%
          dplyr::filter(
            .data$freq >= min(private$.inputPanel$inputValues$freqSlider) &
            .data$freq <= max(private$.inputPanel$inputValues$freqSlider)
          )

        private$.widget$data <- data
        private$.table$data <- data
      })

      observeEvent(private$.table$bindings$rows_all, {
        data <- private$.table$data %>%
          dplyr::filter(
            dplyr::row_number() %in% private$.table$bindings$rows_all
          )
        private$.widget$data <- data
      })
    },

    getNone = function() {
      none <- if (private$.inputPanel$inputValues$none) {
        ""
      } else {
        "None"
      }
      return(none)
    },

    getCombinations = function() {
      if (is.null(private$.inputPanel$inputValues$groupCombi)) {
        return(FALSE)
      } else {
        return(private$.inputPanel$inputValues$groupCombi)
      }
    }
  )
)
