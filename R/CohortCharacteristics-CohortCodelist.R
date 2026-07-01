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

#' @title CohortCodelist Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortCodelist module that shows characteristics results (table) from the CohortCharacteristics package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#' if (interactive()) {
#'   library(DarwinShinyModules)
#'
#'   result <- CohortCharacteristics::summariseCohortCodelist(cdm$my_cohort)
#'
#'   mod <- CohortCodelist$new(result)
#'
#'   preview(mod)
#' }
#' }
#' }
CohortCodelist <- R6::R6Class(
  classname = "CohortCodelist",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result object from `CohortCharacteristics::summariseCodelist()`.
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
    #' @param result (`summarised_result`) Result object from `CohortCharacteristics::summariseCodelist()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)

      self$result <- result
      private$.cdmNames <- getCDMNames(result)
      private$.cohortNames <- getCohortNames(result)

      private$.table <- Flextable$new(
        fun = visOmopResults::visOmopTable,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
    }
  ),

  # private ----
  private = list(
    .result = NULL,

    .table = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .UI = function() {
      shiny::fluidPage(
        private$.generalSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            private$.table$UI()
          )
        )
      )
    },

    .generalSettings = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmNames"),
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
            inputId = shiny::NS(self$namespace, "cohortNames"),
            label = "Target Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        mirai::mirai({
          result |>
            qs2::qs_deserialize() |>
            dplyr::filter(.data$cdm_name %in% cdmName) |>
            omopgenerics::filterGroup(.data$cohort_name %in% cohortName)
        },
        result = private$.result,
        cdmName = input$cdmNames,
        cohortName = input$cohortNames
        )
      })

      shiny::observe({
        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.table$server(input, output, session)
          }
        )
      })
    },

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assertClass(result, "summarised_result", add = collection)
      checkmate::assertTRUE(attr(result, "settings")$result_type == "summarise_cohort_codelist", add = collection)
      checkmate::reportAssertions(collection)
    }
  )
)

# Functions ----
#' moduleCohortCodelist
#'
#' @param result (`summarised_result`) Result from the `summariseCohortCodelist()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortCodelist(result)
#' }
moduleCohortCodelist <- function(result, .softValidation = FALSE) {
  assertType(result, type = "summarise_cohort_codelist")
  checkCDMNames(result, .softValidation)
  CohortCodelist$new(result)
}

#' shinyCohortCodelist
#'
#' @param result (`summarised_result`) Result from the `summariseCohortCodelist()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `shiny.appojb`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleCohortCodelist(result)
#' }
shinyCohortCodelist <- function(result, .softValidation = FALSE) {
  launchBslibApp(
    list(
      CohortCodelist = moduleCohortCodelist(result, .softValidation)
    )
  )
}
