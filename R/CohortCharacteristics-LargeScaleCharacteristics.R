LargeScaleCharacteristics <- R6::R6Class(
  classname = "LargeScaleCharacteristics",
  inherit = DarwinShinyModules::ShinyModule,

  active = list(
    data = function() {
      return(private$.data)
    },
    table = function() {
      return(private$.table)
    }
  ),

  public = list(
    initialize = function(data, ...) {
      super$initialize(...)
      if ("summarised_result" %in% class(data)) {
        private$.data <- data
        private$.tidyData <- omopgenerics::tidy(data)
      } else {
        stop("Data has to be of class: `summarised_result`")
      }

      private$initInputPanel()
      private$initTable()
      private$initTopTable()
      private$initPlot()
      private$initComparePlot()
    }
  ),
  private = list(
    .data = NULL,
    .tidyData = NULL,

    # Nested modules
    .inputPanel = NULL,
    .table = NULL,
    .topTable = NULL,
    .plot = NULL,
    .comparePlot = NULL,

    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Large Scale Characteristics",
              shiny::column(width = 3, private$.inputPanel$UI()),
              shiny::column(width = 9, private$.table$UI())
          ),
          shiny::tabPanel(
            title = "Top Concepts",
            shinyWidgets::pickerInput(
              inputId = shiny::NS(self$namespace, "top_n"),
              label = "Top Concepts",
              choices = c(5, 10, 25, 50, 100),
              selected = 5
            ),
            private$.topTable$UI()
          ),
          shiny::tabPanel(
            title = "Plot",
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "plot_facet"),
                label = "Facet",
                choices = CohortCharacteristics::availablePlotColumns(private$.data),
                selected = NULL,
                multiple = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "plot_colour"),
                label = "Colour",
                choices = c("cdm_name", "cohort_name", "variable_level", "type"),
                selected = NULL,
                multiple = TRUE
              )
            ),
            shiny::column(width = 9, private$.plot$UI())
          ),
          shiny::tabPanel(
            title = "Compare Plot",
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "c_plot_colour"),
                label = "Colour",
                choices = c("cdm_name", "cohort_name", "variable_level", "type"),
                selected = "cdm_name",
                multiple = FALSE
              ),
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "c_plot_reference"),
                label = "Reference",
                choices = private$.data |>
                  dplyr::select(dplyr::contains("_level")) |>
                  tidyr::pivot_longer(cols = names(private$.data)[grep("_level", x = names(private$.data))]) |>
                  dplyr::pull(.data$value) |>
                  unique(),
                selected = "overall",
                multiple = FALSE
              ),
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "c_plot_facet"),
                label = "Facet",
                choices = CohortCharacteristics::availablePlotColumns(private$.data),
                selected = NULL,
                multiple = TRUE
              )
            ),
            shiny::column(width = 9, private$.comparePlot$UI())
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)

      shiny::observeEvent(list(
        private$.inputPanel$inputValues$compareBy,
        private$.inputPanel$inputValues$hide,
        private$.inputPanel$inputValues$smdReference
      ), {
        private$.table$args$compareBy <- private$.inputPanel$inputValues$compareBy |>
          private$checkNone()
        private$.table$args$hide <- private$.inputPanel$inputValues$hide
        private$.table$args$smdReference <- private$.inputPanel$inputValues$smdReference |>
          private$checkNone()

        private$.table$server(input, output, session)
      })

      shiny::observeEvent(input$top_n, {
        private$.topTable$args$topConcepts <- input$top_n
        private$.topTable$server(input, output, session)
      })

      shiny::observeEvent(list(
        input$plot_facet,
        input$plot_colour
      ), {
        private$.plot$args$facet <- input$plot_facet
        private$.plot$args$colour <- input$plot_colour
        private$.plot$server(input, output, session)
      })

      shiny::observeEvent(list(
        input$c_plot_colour,
        input$c_plot_reference,
        input$c_plot_facet
      ), {
        private$.comparePlot$args$colour <- input$c_plot_colour
        private$.comparePlot$args$reference <- input$c_plot_reference
        private$.comparePlot$args$facet <- input$c_plot_facet
        private$.comparePlot$server(input, output, session)
      })
    },

    checkNone = function(val) {
      if (val == "none") {
        NULL
      } else {
        val
      }
    },

    initInputPanel = function() {
      compareByChoices <- c(
        "cdm_name", "cohort_name", "variable_level", "type",
        omopgenerics::strataColumns(private$.data)
      )

      smdReferenceChoices <- private$.tidyData |>
        dplyr::select(compareByChoices) |>
        tidyr::pivot_longer(cols = compareByChoices) |>
        dplyr::pull(.data$value) |>
        unique()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          compareBy = shinyWidgets::pickerInput,
          smdReference = shinyWidgets::pickerInput,
          hide = shinyWidgets::pickerInput
        ),
        args = list(
          compareBy = list(
            inputId = "compareBy",
            label = "Compare By",
            choices = c(compareByChoices, "none"),
            selected = "none",
            multiple = FALSE
          ),
          smdReference = list(
            inputId = "smdReference",
            label = "SMD Reference",
            choices = c(smdReferenceChoices, "none"),
            selected = "none",
            multiple = FALSE
          ),
          hide = list(
            inputId = "hide",
            label = "Columns to hide",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = NULL,
            multiple = TRUE
          )
        ),
        parentNamespace = self$namespace
      )
    },
    initTable = function() {
      private$.table <- DTTable$new(
        fun = CohortCharacteristics::tableLargeScaleCharacteristics,
        args = list(result = private$.data, type = "DT"),
        parentNamespace = self$namespace
      )
    },

    initTopTable = function() {
      private$.topTable <- GTTable$new(
        fun = CohortCharacteristics::tableTopLargeScaleCharacteristics,
        args = list(result = private$.data),
        parentNamespace = self$namespace
      )
    },
    initPlot = function() {
      private$.plot <- PlotPlotly$new(
        fun = CohortCharacteristics::plotLargeScaleCharacteristics,
        args = list(result = private$.data),
        parentNamespace = self$namespace
      )
    },
    initComparePlot = function() {
      private$.comparePlot <- PlotPlotly$new(
        fun = CohortCharacteristics::plotComparedLargeScaleCharacteristics,
        args = list(result = private$.data),
        parentNamespace = self$namespace
      )
    }
  )
)
