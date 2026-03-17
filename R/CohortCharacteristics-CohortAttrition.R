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

#' @title CohortAttrition Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortAttrition module that displays tables and plots of the
#' `summarised_result` object created by `CohortCharacteristics::summariseCohortAttrition()`.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   CDMConnector::requireEunomia()
#'   con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
#'   cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")
#'
#'   cdm <- DrugUtilisation::generateIngredientCohortSet(cdm = cdm, name = "my_cohort", ingredient = c("warfarin", "acetaminophen"))
#'
#'   summarisedAttrition <- CohortCharacteristics::summariseCohortAttrition(cdm$my_cohort)
#'
#'   mod <- Attrition$new(result = summarisedAttrition)
#'   DarwinShinyModules::preview(mod)
#' }
CohortAttrition <- R6::R6Class(
  classname = "CohortAttrition",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`)
    result = function() {
      return(private$.result)
    },

    #' @field cdmNames (`character(n)`)
    cdmNames = function() {
      return(private$.cdmNames)
    },

    #' @field cohortNames (`character(n)`)
    cohortNames = function() {
      return(private$.cohortNames)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method.
    #'
    #' @param result (`summarised_result`) Object created by `CohortCharacteristics::summariseLargeScaleCharacteristics()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)
      private$.result <- result
      private$.widget <- DarwinShinyModules::PlotWidget$new(
        fun = CohortCharacteristics::plotCohortAttrition,
        args = list(
          result = private$.result
        ),
        title = NULL,
        parentNamespace = self$namespace
      )

      private$.table <- DarwinShinyModules::Flextable$new(
        fun = CohortCharacteristics::tableCohortAttrition,
        args = list(
          result = private$.result,
          type = "flextable",
          style = "darwin"
        ),
        parentNamespace = self$namespace
      )

      private$.getCDMNames()
      private$.getCohortNames()

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .result = NULL,

    .widget = NULL,
    .table = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,

    ## UI ----
    .UI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shiny::selectInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "Cohort Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1]
          ),
          shiny::selectInput(
            inputId = shiny::NS(self$namespace, "cohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1]
          )
        ),
        shiny::column(
          width = 5,
          private$.widget$UI()
        ),
        shiny::column(
          width = 5,
          private$.table$UI()
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      shiny::observeEvent(list(input$cdmName, input$cohortName), {
        dataSubset <- private$.result |>
          dplyr::filter(.data$cdm_name == input$cdmName) |>
          omopgenerics::filterGroup(.data$cohort_name == input$cohortName)

        private$.widget$args$result <- dataSubset
        private$.table$args$result <- dataSubset

        private$.widget$server(input, output, session)
        private$.table$server(input, output, session)
      })
    },

    ## Helpers ----
    .getCDMNames = function() {
      private$.cdmNames <- private$.result |>
        dplyr::distinct(.data$cdm_name) |>
        dplyr::pull(.data$cdm_name)
    },

    .getCohortNames = function() {
      private$.cohortNames <- private$.result |>
        dplyr::filter(.data$group_name == "cohort_name") |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)
    }
  )
)
