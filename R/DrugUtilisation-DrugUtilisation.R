DrugUtilisation <- R6::R6Class(
  classname = "DrugUtilisation",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    result = function() {
      return(private$.result)
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)
      private$.result <- result

      private$.setFilterValues()

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableDrugUtilisation,
        args = list(style = "darwin", type = "flextable"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotPlotly$new(
        fun = DrugUtilisation::plotDrugUtilisation,
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

    .cdmNames = NULL,
    .cohortNames = NULL,
    .strata = NULL,
    .variableNames = NULL,
    .estimates = NULL,

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        private$.uiGeneralInputs(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            shiny::column(
              width = 2,
              private$.uiTableInput()
            ),
            shiny::column(
              width = 10,
              private$.table$UI()
            )
          ),
          shiny::tabPanel(
            title = "Plot",
            shiny::column(
              width = 2,
              private$.uiPlotInput()
            ),
            shiny::column(
              width = 10,
              private$.plot$UI()
            )
          )
        )
      )
    },

    .uiGeneralInputs = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "cdmName"),
          label = "CDM Name",
          choices = private$.cdmNames,
          selected = private$.cdmNames[1],
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "cohortName"),
          label = "Cohort Name",
          choices = private$.cohortNames,
          selected = private$.cohortNames[1],
          multiple = TRUE
        )
      )
    },

    .uiTableInput = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "strata"),
          label = "Strata",
          choices = private$.strata,
          selected = private$.strata[1],
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "header"),
          label = "Headers",
          choices = availableTableColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "groupColumn"),
          label = "Group Columns",
          choices = availableTableColumns(private$.result),
          multiple = TRUE
        )
      )
    },

    .uiPlotInput = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "variable"),
          label = "Variable",
          choices = private$.variableNames,
          selected = private$.variableNames[1]
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "plotType"),
          label = "Plot Type",
          choices = c("scatterplot", "barplot", "boxplot"),
          selected = "barplot"
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetX"),
          label = "Horizontal Facet",
          choices = availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetY"),
          label = "Vertical Facet",
          choices = availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "colour"),
          label = "Colour",
          choices = availablePlotColumns(private$.result)
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "estimates"),
          label = "Estimates",
          choices = private$.estimates,
          selected = private$.estimates[1]
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.serverTable(input, output, session)
      private$.serverPlot(input, output, session)
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName, input$cohortName,
        input$strata, input$header, input$groupColumn
      ), {
        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$strata_name %in% input$strata
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn

        private$.table$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName, input$cohortName,
        input$variable, input$plotType, input$facetX, input$facetY, input$colour, input$estimates
      ), {
        private$.plot$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$estimate_name %in% input$estimates
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.plot$args$facet <- makeFacetFormula(input$facetX, input$facetY)
        private$.plot$args$colour <- input$colour
        private$.plot$args$plotType <- input$plotType
        private$.plot$args$variable <- input$variable

        private$.plot$server(input, output, session)
      })
    },

    ## Helpers ----
    .setFilterValues = function() {
      private$.cdmNames <- getCDMNames(private$.result)
      private$.cohortNames <- getCohortNames(private$.result)
      private$.strata <- getStrata(private$.result)

      private$.variableNames <- private$.result |>
        dplyr::distinct(.data$variable_name) |>
        dplyr::pull(.data$variable_name)

      private$.estimates <- private$.result |>
        dplyr::distinct(.data$estimate_name) |>
        dplyr::pull(.data$estimate_name)
    }
  )
)
