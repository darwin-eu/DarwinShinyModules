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

#' @title DoseCoverage Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' DoseCoverage module that shows a table
#'
#' @export
#'
#' @examples{
#' if (interactive()) {
#'   library(DarwinShinyModules)
#'   library(DrugUtilisation)
#'
#'   cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 100)
#'
#'   res <- cdm$cohort1 |>
#'     DrugUtilisation::summariseProportionOfPatientsCovered(followUpDays = 365)
#'
#'   mod <- DoseCoverage$new(res)
#'   preview(mod)
#' }
#' }
DoseCoverage <- R6::R6Class(
  classname = "DoseCoverage",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result
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
    #' Initializer method.
    #'
    #' @param result (`summarised_result`) Object created by `DrugUtilisation::summariseDoseCoverage()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)

      private$.checkResult(result)

      private$.cdmNames <- getCDMNames(result)
      private$.strataCols <- DrugUtilisation::strataColumns(result)
      private$.strataGroups <- result |>
        dplyr::distinct(.data$strata_name) |>
        dplyr::pull()
      private$.tableCols <- availableTableColumns(result)
      private$.result <- qs2::qs_serialize(result)

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableDoseCoverage,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
      return(self)
    }
  ),

  # Private ----
  private = list(
    .result = NULL,

    .tableCols = NULL,
    .cdmNames = NULL,
    .strataCols = NULL,
    .strataGroups = NULL,

    .table = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assert_class(x = result, classes = "summarised_result", add = collection)
      checkmate::assert_true(attr(result, "settings")$result_type == "summarise_dose_coverage", add = collection)
      checkmate::reportAssertions(collection)
    },

    .UI = function() {
      shiny::fluidPage(
        private$.uiGeneralSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title  = "Table",
            private$.uiTable()
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
            inputId = shiny::NS(self$namespace, "strata"),
            label = "Strata",
            choices = private$.strataGroups,
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
            choices = private$.tableCols,
            selected = c("variable_name", "estimate_name"),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Column",
            choices = private$.tableCols,
            selected = c("cdm_name", "ingredient_name"),
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

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        mirai::mirai({
          result |>
            qs2::qs_deserialize() |>
            dplyr::filter(
              .data$cdm_name %in% cdmName,
              .data$strata_name %in% strata
            )
        },
        result = private$.result,
        cdmName = input$cdmName,
        strata = input$strata
        )
      })

      shiny::observe({
        private$.table$args["header"] <- list(input$header)
        private$.table$args["groupColumn"] <- list(input$groupColumn)

        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.table$server(input, output, session)
          }
        )
      })
    }
  )
)

# Functions ----
#' moduleDoseCoverage
#'
#' @param result (`summarised_result`) Result from the `summariseDoseCoverage` function from the DrugUtilisation pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleDoseCoverage(result)
#' }
moduleDoseCoverage <- function(result, .softValidation = FALSE) {
  assertType(result, "summarise_dose_coverage")
  checkCDMNames(result, .softValidation)
  DoseCoverage$new(result)
}

#' shinyDoseCoverage
#'
#' @param result (`summarised_result`) Result from the `summariseDoseCoverage` function from the DrugUtilisation pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinyDoseCoverage(result)
#' }
shinyDoseCoverage <- function(result, .softValidation = FALSE) {
  launchBslibApp(
    list(
      DoseCoverage = moduleDoseCoverage(result, .softValidation)
    )
  )
}
