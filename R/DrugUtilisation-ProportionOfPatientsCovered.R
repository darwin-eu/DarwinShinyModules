ProportionOfPatientsCovered <- R6::R6Class(
  classname = "ProportionOfPatientsCovered",
  inherit = ShinyModule,

  active = list(),

  public = list(
    initialize = function(result, ...) {
      checkmate::assert_class(x = result, classes = "summarised_result")
      checkmate::assert_true(attr(result, "settings")$result_type == "summarise_proportion_of_patients_covered")

      private$.result <- result

      private$.cdmNames <- getCDMNames(private$.result)
      private$.cohortNames <- getCohortNames(private$.result)

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableProportionOfPatientsCovered,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = DrugUtilisation::plotProportionOfPatientsCovered,
        args = list(style = "darwin"),
        parentNamespace = self$namespace
      )
    }
  ),

  private = list(
    .result = NULL,
    .table = NULL,
    .plot = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .UI = function(input, output, session) {
      shiny::fluidPage(
        private$.uiGeneralSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            private$.uiTable()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.uiPlot()
          )
        )
      )
    },

    .uiGeneralSettings = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "CDM Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .uiTable = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "header"),
            label = "Header",
            choices = availableTableColumns(result),
            selected = c("cohort_name", DrugUtilisation::strataColumns(result)),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Column",
            choices = availableTableColumns(result),
            selected = "cdm_name",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shiny::actionButton(
            inputId = shiny::NS(self$namespace, "submitTable"),
            label = "",
            icon = shiny::icon("refresh")
          )
        ),
        shiny::column(
          width = 10,
          shinyWidgets::addSpinner(
            private$.table$UI()
          )
        )
      )
    },

    .uiPlot = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "facet"),
            label = "facet",
            choices = availablePlotColumns(private$.result),
            selected = "cohort_name",
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "colour"),
            label = "colour",
            choices = availablePlotColumns(private$.result),
            selected = DrugUtilisation::strataColumns(private$.result),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "ribbon"),
            label = "ribbon",
            choices = c("On", "Off"),
            selected = c("Yes")
          ),
          shiny::actionButton(
            inputId = shiny::NS(self$namespace, "submitPlot"),
            label = "",
            icon = shiny::icon("refresh")
          )
        ),
        shiny::column(
          width = 10,
          shinyWidgets::addSpinner(
            private$.plot$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        private$.table$args$result <- private$.result |>
          dplyr::filter(.data$cdm_name %in% input$cdmName) |>
          omopgenerics::filterGroup(
            .data$cohort_name %in% input$cohortName
          )
      })

      shiny::observeEvent(input$submitTable, {
        private$.table$args["header"] <- list(input$header)
        private$.table$args["groupColumn"] <- list(input$groupColumn)
        private$.table$args$result <- fetchData()
        private$.table$server(input, output, session)
      })

      shiny::observeEvent(input$submitPlot, {
        private$.plot$args["facet"] <- list(input$facet)
        private$.plot$args["colour"] <- list(input$colour)
        private$.plot$args["ribbon"] <- list(
          convertLabelToLogical(input$ribbon, trueVal = "On", falseVal = "Off")
        )
        private$.plot$args$result <- fetchData()
        private$.plot$server(input, output, session)
      })
    }
  )
)

# source("./R/utils.R")
# source("./R/utils-SummarizedResult.R")
#
# mod <- ProportionOfPatientsCovered$new(result)
#
# preview(mod)
