# Copyright 2024 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title Characterisitcs Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Characteristics module that shows characteristics results (table and plot) from the CohortCharacteristics package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "CohortCharacteristics",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     result <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/CohortCharacteristics/1.1.1/characteristics.csv"
#'     ))
#'
#'     charMod <- Characteristics$new(result = result)
#'
#'     ui <- shiny::fluidPage(
#'       charMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       charMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
Characteristics <- R6::R6Class(
  classname = "Characteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result of `CohortCharacteristics::summariseCharacteristics()`.
    result = function(result) {
      if (missing(result)) {
        return(private$.result)
      } else {
        collection <- checkmate::makeAssertCollection()
        checkmate::assertClass(x = result, classes = c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"), add = collection)
        try({
          settings <- omopgenerics::settings(result)
          checkmate::assertTRUE(settings$result_type == "summarise_characteristics", add = collection)
          checkmate::assertTRUE(settings$package_name == "CohortCharacteristics", add = collection)
        }, silent = TRUE)
        checkmate::reportAssertions(collection)
        private$.result <- result
      }
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param result (`summarised_result`) Result of `CohortCharacteristics::summariseCharacteristics()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result = NULL, ...) {
      private$assertInstall("CohortCharacteristics", "1.0.0")
      super$initialize(...)
      private$.result <- result

      private$.setFilterValues()

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

      return(invisible(self))
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
    .estimateNames = NULL,

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
          )
        )
      )
    },

    .tableUI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCDMName"),
            label = "CDM Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = TRUE,
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableStrata"),
            label = "Strata",
            choices = private$.strata,
            selected = private$.strata[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableVariable"),
            label = "Variables",
            choices = private$.variableNames,
            selected = private$.variableNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableHeader"),
            label = "Headers",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            selected = c("cdm_name", "cohort_name"),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableGroupColumn"),
            label = "Group Columns",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
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
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotCDMName"),
            label = "CDM Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = TRUE,
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotCohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotStrata"),
            label = "Strata",
            choices = private$.strata,
            selected = private$.strata[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotVariableName"),
            label = "Variable",
            choices = private$.variableNames,
            selected = private$.variableNames[1]
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotType"),
            label = "Plot Type",
            choices = c("barplot", "scatterplot", "boxplot"),
            selected = "boxplot"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotEstimateName"),
            label = "Estimate",
            choices = private$.estimateNames,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotColour"),
            label = "Colour",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetX"),
            label = "Horizontal Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetY"),
            label = "Vertical Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 10,
          private$.plot$UI()
        )
      )
    },

    .server = function(input, output, session) {
      output$foo <- shiny::renderText({
        input$foo
      })
      private$.tableServer(input, output, session)
      private$.plotServer(input, output, session)
    },

    .tableServer = function(input, output, session) {
      shiny::observeEvent(list(
        input$tableCDMName, input$tableCohortName,
        input$tableStrata, input$tableVariable,
        input$tableHeader, input$tableGroupColumn
      ), {
        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$tableCDMName,
            .data$variable_name %in% input$tableVariable,
            .data$strata_name %in% input$tableStrata
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$tableCohortName)

        private$.table$args$header <- input$tableHeader
        private$.table$args$groupColumn <- input$tableGroupColumn
        private$.table$server(input, output, session)
      })
    },

    .plotServer = function(input, output, session) {
      shiny::observeEvent(list(
        input$plotCDMName, input$plotCohortName, input$plotStrata, input$plotVariableName
      ), {
        private$.plot$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$plotCDMName,
            .data$variable_name %in% input$plotVariableName,
            .data$strata_name %in% input$plotStrata
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$plotCohortName)

        choices <- private$.plot$args$result |>
          dplyr::distinct(.data$estimate_name) |>
          dplyr::pull(.data$estimate_name)

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "plotEstimateName",
          choices = choices,
          selected = choices
        )
      }, ignoreNULL = TRUE)

      shiny::observeEvent(list(
        input$plotCDMName, input$plotCohortName, input$plotStrata,
        input$plotVariableName, input$plotEstimateName, input$plotType,
        input$plotColour, input$plotFacetX, input$plotFacetY
      ), {
        private$.plot$args$facet <- makeFacetFormula(input$plotFacetX, input$plotFacetY)
        private$.plot$args$colour <- input$plotColour
        private$.plot$args$plotType <- input$plotType
        private$.plot$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$plotCDMName,
            .data$variable_name %in% input$plotVariableName,
            .data$estimate_name %in% input$plotEstimateName,
            .data$strata_name %in% input$plotStrata
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$plotCohortName)

        private$.plot$server(input, output, session)
      })
    },

    .setFilterValues = function() {
      private$.cdmNames <- private$.result |>
        dplyr::distinct(.data$cdm_name) |>
        dplyr::pull(.data$cdm_name)

      private$.cohortNames <- private$.result |>
        dplyr::filter(.data$group_name == "cohort_name") |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)

      private$.strata <- private$.result |>
        dplyr::distinct(.data$strata_name) |>
        dplyr::pull(.data$strata_name)

      private$.variableNames <- private$.result |>
        dplyr::distinct(.data$variable_name) |>
        dplyr::pull(.data$variable_name)

      private$.estimateNames <- private$.result |>
        dplyr::distinct(.data$estimate_name) |>
        dplyr::pull(.data$estimate_name)
    }
  )
)
