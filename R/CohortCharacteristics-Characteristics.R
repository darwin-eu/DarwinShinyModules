Characteristics <- R6::R6Class(
  classname = "Characteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Public ----
  public = list(
    initialize = function(result = NULL, ...) {
      super$initialize(...)
      private$.result <- result

      private$.table <- DarwinShinyModules::Flextable$new(
        fun = CohortCharacteristics::tableCharacteristics,
        args = list(
          result = private$.result,
          type = "flextable",
          style = "darwin"
        ),
        parentNamespace = self$namespace
      )

      private$.plot <- DarwinShinyModules::PlotStatic$new(
        fun = CohortCharacteristics::plotCharacteristics,
        args = list(style = "darwin"),
        parentNamespace = self$namespace
      )
    }
  ),

  # Private ----
  private = list(
    .result = NULL,
    .table = NULL,
    .plot = NULL,

    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          # Table ----
          shiny::tabPanel(
            title = "Table",
            private$.tableUI()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.plotUI()
          ),
          shiny::tabPanel(
            title = "Info",
            private$.infoUI()
          )
        )
      )
    },

    .tableUI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "header"),
            label = "Headers",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            selected = c("cdm_name", "cohort_name"),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Columns",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "hide"),
            label = "Columns to hide",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            selected = c(CohortCharacteristics::additionalColumns(private$.result), CohortCharacteristics::settingsColumns(private$.result)),
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 10,
          private$.table$UI()
        )
      )
    },

    .plotUI = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "plotType"),
          label = "Plot Type",
          choices = c("barplot", "scatterplot", "boxplot"),
          selected = "boxplot"
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facet"),
          label = "Facet",
          choices = CohortCharacteristics::availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "colour"),
          label = "Colour",
          choices = CohortCharacteristics::availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "estimate_name"),
          label = "Estimate",
          choices = unique(private$.result$estimate_name),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "variable_name"),
          label = "Variable",
          choices = unique(private$.result$variable_name),
          selected = unique(private$.result$variable_name)[1]
        ),
        private$.plot$UI()
      )
    },

    .infoUI = function() {
      shiny::markdown("
      ## Characteristics
      The characteristics module contains two sections (`Table` and `Plot`).

      User input in the module reflect the parameters in the[`tableCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/tableCharacteristics.html) and [`plotCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/plotCharacteristics.html) functions from the [`CohortCharacteristics`](https://darwin-eu.github.io/CohortCharacteristics/index.html) package.
      ")
    },

    .server = function(input, output, session) {
      output$foo <- shiny::renderText({
        input$foo
      })
      private$.tableServer(input, output, session)
      private$.plotServer(input, output, session)
    },

    .tableServer = function(input, output, session) {
      shiny::observeEvent(list(input$header, input$groupColumn, input$hide), {
        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn
        private$.table$args$hide <- input$hide
        private$.table$server(input, output, session)
      })
    },

    .plotServer = function(input, output, session) {
      shiny::observeEvent(list(input$plotType, input$facet, input$colour, input$estimate_name, input$variable_name), {
        private$.plot$args$plotType <- input$plotType
        private$.plot$args$facet <- input$facet
        private$.plot$args$colour <- input$colour

        estimate_name <- if (is.null(input$estimate_name)) unique(private$.result$estimate_name)

        private$.plot$args$result <- private$.result |>
          dplyr::filter(.data$estimate_name %in% estimate_name) |>
          dplyr::filter(.data$variable_name %in% input$variable_name)

        private$.plot$server(input, output, session)
      })
    }
  ),

  # Active ----
  active = list()
)
