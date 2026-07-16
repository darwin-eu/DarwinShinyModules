# Copyright 2026 DARWIN EU®
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

#' @title CohortCount Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortCount module that shows characteristics results (table and plot) from the CohortCharacteristics package.
#'
#' @export
#'
#' @examples{
#' if (interactive()) {
#'   library(DarwinShinyModules)
#'
#'   cdm <- CohortCharacteristics::mockCohortCharacteristics()
#'
#'   result <- cdm$cohort1 |>
#'     PatientProfiles::addSex() |>
#'     CohortCharacteristics::summariseCohortCount(strata = "sex")
#'
#'   mod <- CohortCount$new(result)
#'
#'   preview(mod)
#' }
#' }
CohortCount <- R6::R6Class(
  classname = "CohortCount",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result of `CohortCharacteristics::summariseCohortCount()`.
    result = function(result) {
      if (missing(result)) {
        return(qs2::qs_deserialize(private$.result))
      } else {
        private$.checkResult(result)
        private$.result <- qs2::qs_serialize(result)
      }
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param result (`summarised_result`) Result of `CohortCharacteristics::summariseCohortCount()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)

      self$result <- result

      private$.cdmNames <- getCDMNames(result)
      private$.cohortNames <- getCohortNames(result)
      private$.strataNames <- unique(result$strata_name)
      private$.plotVariables <- unique(result$variable_name)
      private$.plotColumns <- availablePlotColumns(result)
      private$.tableColumns <- availableTableColumns(result)

      private$.plotXChoices <- result |>
        dplyr::select(dplyr::ends_with("_name"), -"variable_name", -"cdm_name") |>
        tidyr::pivot_longer(cols = dplyr::ends_with("_name")) |>
        dplyr::filter(.data$value != "overall") |>
        dplyr::distinct(.data$value) |>
        dplyr::pull(.data$value)

      private$.plotXChoices <- c("cdm_name", "variable_name", private$.plotXChoices)

      private$.table <- Flextable$new(
        fun = CohortCharacteristics::tableCohortCount,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = CohortCharacteristics::plotCohortCount,
        args = list(style = "darwin"),
        height = "80vh",
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
    .strataNames = NULL,
    .plotVariables = NULL,
    .plotXChoices = NULL,
    .plotColumns = NULL,
    .tableColumns = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .UI = function() {
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
            inputId = shiny::NS(self$namespace, "cdmNames"),
            label = "Data Source",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1:5],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cohortNames"),
            label = "Cohort Names",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "strataNames"),
            label = "Strata",
            choices = private$.strataNames,
            selected = "overall",
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .uiTable = function() {
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "header"),
            label = "Header",
            choices = private$.tableColumns,
            selected = "cohort_name",
            multiple = TRUE,
            options = private$.pikcerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Column",
            choices = private$.tableColumns,
            selected = character(),
            multiple = TRUE,
            options = private$.pikcerOptions
          )
        ),
        shiny::column(
          width = 10,
          private$.table$UI()
        )
      )
    },

    .uiPlot = function() {
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "variable"),
            label = "Variable",
            choices = private$.plotVariables,
            selected = private$.plotVariables[1]
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "x"),
            label = "X-axis",
            choices = private$.plotXChoices,
            selected = "cohort_name"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "facet"),
            label = "facet",
            choices = private$.plotColumns,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "colour"),
            label = "colour",
            choices = private$.plotColumns,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::column(
          width = 10,
          private$.plot$UI()
        )
      )
    },

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        mirai::mirai({
          result |>
            qs2::qs_deserialize() |>
            dplyr::filter(
              .data$cdm_name %in% cdmNames,
              .data$strata_name %in% strataNames
            ) |>
            omopgenerics::filterGroup(.data$cohort_name %in% cohortNames)
        }, result = private$.result,
        cdmNames = input$cdmNames,
        cohortNames = input$cohortNames,
        strataNames = input$strataNames
        )
      })

      shiny::observe({
        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn

        private$.plot$args$x <- input$x
        private$.plot$args$facet <- input$facet
        private$.plot$args$colour <- input$colour

        promises::then(
          fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.table$server(input, output, session)

            private$.plot$args$result <- result |>
              dplyr::filter(.data$variable_name %in% input$variable)
            private$.plot$server(input, output, session)
          }
        )
      })
    },

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assertClass(result, "summarised_result", add = collection)
      checkmate::assertTRUE(attr(result, "settings")$result_type == "summarise_cohort_count")
      checkmate::reportAssertions(collection)
    }
  )
)

# Functions ----
#' moduleCohortCount
#'
#' @param result (`summarised_result`) Result from the `summariseCohortCount()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortCount(result)
#' }
moduleCohortCount <- function(result, .softValidation = FALSE) {
  assertType(result, type = "summarise_cohort_count")
  checkCDMNames(result, .softValidation)
  CohortCount$new(result)
}

#' shinyCohortCount
#'
#' @param result (`summarised_result`) Result from the `summariseCohortCount()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `shiny.appojb`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortCount(result)
#' }
shinyCohortCount <- function(result, .softValidation = FALSE) {
  launchBslibApp(
    list(
      Characteristics = moduleCohortCount(result, .softValidation)
    )
  )
}
