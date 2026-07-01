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

#' @title CohortOverlap Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortOverlap module that shows cohort overlap results (table and plot) from the CohortCharacteristics package.
#'
#' @export
#'
#' @examples{
#' if (interactive()) {
#'   library(CohortCharacteristics)
#'   library(DarwinShinyModules)
#'
#'   cdm <- mockCohortCharacteristics()
#'
#'   result <- summariseCohortOverlap(cdm$cohort2)
#'
#'   mod <- CohortOverlap$new(result)
#'
#'   preview(mod)
#' }
#' }
CohortOverlap <- R6::R6Class(
  classname = "CohortOverlap",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result of `CohortCharacteristics::summariseCohortOverlap()`.
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
    #' @param result (`summarised_result`) Result of `CohortCharacteristics::summariseCohortOverlap()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)
      self$result <- result

      private$.cdmNames <- getCDMNames(result)

      cohortNames <- result |>
        omopgenerics::splitGroup() |>
        dplyr::select("cohort_name_reference", "cohort_name_comparator")

      private$.refCohortNames <- cohortNames |>
        dplyr::distinct(.data$cohort_name_reference) |>
        dplyr::pull()

      private$.compCohortNames <- cohortNames |>
        dplyr::distinct(.data$cohort_name_comparator) |>
        dplyr::pull()

      private$.strataNames <- unique(result$strata_name)

      private$.tableColumns <- availableTableColumns(result)
      private$.plotColumns <- availablePlotColumns(result)

      private$.table <- Flextable$new(
        fun = CohortCharacteristics::tableCohortOverlap,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = CohortCharacteristics::plotCohortOverlap,
        args = list(style = "darwin"),
        parentNamespace = self$namespace,
        height = "80vh"
      )

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .result = NULL,

    .cdmNames = NULL,
    .refCohortNames = NULL,
    .compCohortNames = NULL,
    .strataNames = NULL,

    .tableColumns = NULL,
    .plotColumns = NULL,

    .table = NULL,
    .plot = NULL,

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
            inputId = shiny::NS(self$namespace, "refCohortNames"),
            label = "Reference Cohort Names",
            choices = private$.refCohortNames,
            selected = private$.refCohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "compCohortNames"),
            label = "Compararotr Cohort Names",
            choices = private$.compCohortNames,
            selected = private$.compCohortNames[1],
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
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "uniqueCombinations"),
            label = "unique Combinations",
            choices = c("Yes", "No"),
            selected = "Yes"
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
            selected = "variable_name",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Column",
            choices = private$.tableColumns,
            selected = "cdm_name",
            multiple = TRUE,
            options = private$.pickerOptions
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
            inputId = shiny::NS(self$namespace, "facet"),
            label = "Facet",
            choices = private$.plotColumns,
            selected = c("cdm_name", "cohort_name_reference"),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "colour"),
            label = "Colour",
            choices = private$.plotColumns,
            selected = "variable_name",
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
            omopgenerics::filterGroup(
              .data$cohort_name_reference %in% refCohortNames,
              .data$cohort_name_comparator %in% compCohortNames
            )
        },
        result = private$.result,
        cdmNames = input$cdmNames,
        compCohortNames = input$compCohortNames,
        refCohortNames = input$refCohortNames,
        strataNames = input$strataNames
        )
      }) |>
        shiny::bindCache(private$.result, input$cdmNames, input$refCohortNames, input$compCohortNames, input$strataNames)

      shiny::observe({
        uniqueCombinations <- convertLabelToLogical(input$uniqueCombinations, trueVal = "Yes", falseVal = "No")
        private$.table$args$uniqueCombinations <- uniqueCombinations
        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn

        private$.plot$args$uniqueCombinations <- uniqueCombinations
        private$.plot$args$facet <- input$facet
        private$.plot$args$colour <- input$colour

        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.plot$args$result <- result

            private$.table$server(input, output, session)
            private$.plot$server(input, output, session)
          }
        )
      })
    },

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assertClass(result, "summarised_result", add = collection)
      checkmate::assertTRUE(attr(result, "settings")$result_type == "summarise_cohort_overlap")
      checkmate::reportAssertions(collection)
    }
  )
)

# Functions ----
#' moduleCohortOverlap
#'
#' @param result (`summarised_result`) Result from the `summariseCohortOverlap()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortOverlap(result)
#' }
moduleCohortOverlap <- function(result, .softValidation = FALSE) {
  assertType(result, type = "summarise_cohort_overlap")
  checkCDMNames(result, .softValidation)
  CohortOverlap$new(result)
}

#' shinyCohortOverlap
#'
#' @param result (`summarised_result`) Result from the `summariseCohortOverlap()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `shiny.appojb`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortOverlap(result)
#' }
shinyCohortOverlap <- function(result, .softValidation = FALSE) {
  launchBslibApp(
    list(
      CohortOverlap = moduleCohortOverlap(result, .softValidation)
    )
  )
}
