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

#' @title CohortSurvival Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortSurvival module that shows a that supports results from the
#' CohortSurvival package.
#'
#' @details
#' The module consists of the following:
#' \describe{
#'   \item{"PlotPlotly"}{Interactive Plotly plot, visualizing the data.}
#'   \item{"GTTable"}{gttable visualizing the tidy data}
#'   \item{"InputPanel"}{Input panel dealing with user input}
#'   \item{"Table"}{basic table visualizing the raw data}
#' }
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "CohortSurvival",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     cdm <- CohortSurvival::mockMGUS2cdm()
#'
#'     result <- CohortSurvival::estimateSingleEventSurvival(
#'       cdm,
#'       targetCohortTable = "mgus_diagnosis",
#'       outcomeCohortTable = "death_cohort",
#'       strata = list(
#'         c("age_group"),
#'         c("sex"),
#'         c("age_group", "sex")
#'       )
#'     )
#'
#'     survMod <- DarwinShinyModules::CohortSurvival$new(result = result)
#'     if (interactive()) {
#'       DarwinShinyModules::preview(survMod)
#'     }
#'   }
#' }
#' }
CohortSurvival <- R6::R6Class(
  classname = "CohortSurvival",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field result (`SummarisedResult`) Summarised result object from `CohortSurvival`
    result = function(result) {
      if (missing(result)) {
        return(private$.result)
      }
    },

    #' @field table (`Table`) Table module using `CohortSurvival::tableSurvival()`
    table = function() {
      return(private$.table)
    },

    #' @field tableEvents (`Table`) Table module using `CohortSurvival::tableSurvivalEvents()`
    tableEvents = function() {
      return(private$.tableEvents)
    },

    #' @field plot (`Plot`) Plot module. using `CohortSurvival::plotSurvival()`
    plot = function() {
      return(private$.plot)
    },

    #' @field tableAttrition (`Table`) Table module using `CohortSurvival::tableAttrition()`
    tableAttrition = function() {
      return(private$.tableAttrition)
    },

    #' @field cdmNames (`character`) Available CDM names
    cdmNames = function() {
      return(private$.cdmNames)
    },

    #' @field cohortNames (`character`) Available cohort bames
    cohortNames = function() {
      return(private$.cohortNames)
    },

    #' @field strata (`character`) Available strata names
    strata = function() {
      return(private$.strata)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer function
    #'
    #' @param result (`SummarisedResults`) Summarised result object from `CohortSurvival`
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `invisible(self)`
    initialize = function(result, ...) {
      checkmate::assertClass(x = result, classes = c("summarised_result", "omop_result"))
      super$initialize(...)

      private$.result <- result
      private$.setFilterValues()

      private$.table <- Flextable$new(
        fun = CohortSurvival::tableSurvival,
        args = list(x = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.tableEvents <- Flextable$new(
        fun = CohortSurvival::tableSurvivalEvents,
        args = list(x = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotPlotly$new(
        title = NULL,
        fun = CohortSurvival::plotSurvival,
        args = list(style = "darwin"),
        parentNamespace = self$namespace,
        height = "80vh"
      )

      private$.tableAttrition <- Flextable$new(
        fun = CohortSurvival::tableSurvivalAttrition,
        args = list(result = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,
    .cdmNames = NULL,
    .cohortNames = NULL,
    .outcomeNames = NULL,
    .strata = NULL,

    ### Modules ----
    .table = NULL,
    .tableEvents = NULL,
    .plot = NULL,
    .tableAttrition = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        private$.uiGeneralFilters(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Survival Table",
            private$.uiTable()
          ),
          shiny::tabPanel(
            title = "Survival Events Table",
            private$.uiTableEvents()
          ),
          shiny::tabPanel(
            title = "Kaplan-Meier Plot",
            private$.uiPlot()
          ),
          shiny::tabPanel(
            title = "Attrition Table",
            private$.uiTableAttrition()
          )
        )
      )
    },

    .uiGeneralFilters = function() {
      shiny::fluidPage(
        shiny::h4("General Settings"),
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
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "outcomeNames"),
            label = "Outcome Cohort Name",
            choices = private$.outcomeNames,
            selected = private$.outcomeNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "strata"),
            label = "Strata",
            choices = private$.strata,
            selected = "overall",
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .uiTable = function() {
      shiny::fluidPage(
        shiny::column(
          width = 2,
          shiny::h4("Settings"),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTimeScale"),
            label = "TimeScale",
            choices = c("days", "months", "years"),
            selected = "years"
          ),
          shiny::selectizeInput(
            inputId = shiny::NS(self$namespace, "tableTimes"),
            label = "Times",
            choices = c(1, 3, 5, 6, 7, 10, 12, 365),
            selected = c(1, 3, 5),
            options = list(create = TRUE),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableEstimates"),
            label = "Estimates",
            choices = c(
              "median_survival", "restricted_mean_survival", "q0_survival",
              "q05_survival", "q25_survival", "q75_survival", "q95_survival",
              "q100_survival"
            ),
            selected = c("median_survival", "restricted_mean_survival"),
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

    .uiTableEvents = function() {
      eventGap <- private$.result |>
        omopgenerics::settings() |>
        dplyr::pull(.data$eventgap) |>
        unique() |>
        as.numeric()

      eventGap <- eventGap[!sapply(eventGap, is.na)]

      shiny::fluidPage(
        shiny::column(
          width = 2,
          shiny::h4("Settings"),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTableSurvEventGap"),
            label = "eventGap",
            choices = eventGap,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::column(
          width = 10,
          private$.tableEvents$UI()
        )
      )
    },

    .uiPlot = function() {
      shiny::fluidPage(
        shiny::column(
          width = 2,
          shiny::h4("Settings"),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotRibbon"),
            label = "Ribbon",
            choices = c("On", "Off"),
            selected = "On"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotCumulativeFailure"),
            label = "Cumulative failure",
            choices = c("On", "Off"),
            selected = "Off"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotLogLog"),
            label = "Log-Log Transform",
            choices = c("On", "Off"),
            selected = "Off"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetX"),
            label = "Horizontal facet",
            choices = c("cdm_name", "outcome", "target_cohort", private$.strata),
            selected = "cdm_name",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetY"),
            label = "Vertical facet",
            choices = c("cdm_name", "outcome", "target_cohort", private$.strata),
            selected = "target_cohort",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotColour"),
            label = "Colour",
            choices = c("cdm_name", "outcome", "target_cohort", private$.strata),
            selected = "outcome",
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

    .uiTableAttrition = function() {
      shiny::fluidPage(
        private$.tableAttrition$UI()
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.serverTable(input, output, session)
      private$.serverTableEvents(input, output, session)
      private$.serverPlot(input, output, session)
      private$.serverTableAttrition(input, output, session)
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(
        list(
          input$cdmNames, input$cohortNames, input$outcomeNames, input$strata,
          input$tableTimeScale, input$tableTimes, input$tableEstimates
        ), {
          private$.table$args$timeScale <- input$tableTimeScale
          private$.table$args$times <- as.numeric(input$tableTimes)
          private$.table$args$estimates <- input$tableEstimates

          strata <- if (all(is.null(input$strata))) {
            "overall"
          } else {
            c("overall", input$strata)
          }

          private$.table$args$x <- private$.result |>
            dplyr::filter(
              .data$cdm_name %in% input$cdmNames,
              .data$strata_name %in% strata
            ) |>
            omopgenerics::filterGroup(target_cohort %in% input$cohortNames) |>
            omopgenerics::filterSettings(outcome %in% input$outcomeNames)

          private$.table$server(input, output, session)
        }
      )
    },

    .serverTableEvents = function(input, output, session) {
      shiny::observeEvent(list(input$cdmNames, input$cohortNames, input$outcomeNames, input$strata), {
        strata <- if (all(is.null(input$strata))) {
          "overall"
        } else {
          c("overall", input$strata)
        }

        private$.tableEvents$args$x <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmNames,
            .data$strata_name %in% strata
          ) |>
          omopgenerics::filterGroup(.data$target_cohort %in% input$cohortNames) |>
          omopgenerics::filterSettings(outcome %in% input$outcomeNames)

        private$.tableEvents$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(
        list(
          input$cdmNames,
          input$cohortNames,
          input$outcomeNames,
          input$plotRibbon,
          input$plotCumulativeFailure,
          input$plotLogLog,
          input$plotFacetX,
          input$plotFacetY,
          input$plotColour
        ), {
          inputStrata <- unique(c(input$plotFacetX, input$plotFacetY, input$plotColour))

          strata <- if (all(is.null(inputStrata))) {
            "overall"
          } else {
            c("overall", inputStrata)
          }

          private$.plot$args$result <- private$.result |>
            dplyr::filter(
              .data$cdm_name %in% input$cdmNames,
              .data$strata_name %in% strata
            ) |>
            omopgenerics::filterGroup(target_cohort %in% input$cohortNames) |>
            omopgenerics::filterSettings(outcome %in% input$outcomeNames)

          private$.plot$args$ribbon <- convertLabelToLogical(input$plotRibbon)
          private$.plot$args$cumulativeFailure <- convertLabelToLogical(input$plotCumulativeFailure)
          private$.plot$args$logLog <- convertLabelToLogical(input$plotLogLog)
          private$.plot$args$facet <- makeFacetFormula(facetX = input$plotFacetX, facetY = input$plotFacetY)
          private$.plot$args$colour <- input$plotColour
          private$.plot$server(input, output, session)
        })
    },

    .serverTableAttrition = function(input, output, session) {
      private$.tableAttrition$server(input, outout, session)
    },

    ## Helpers ----
    .setFilterValues = function() {
      private$.cdmNames <- getCDMNames(private$.result)
      private$.strata <- getStrata(private$.result)

      private$.cohortNames <- private$.result |>
        dplyr::filter(.data$group_name == "target_cohort") |>
        dplyr::distinct(.data$group_level) |>
        dplyr::filter(!grepl(.data$group_level, pattern = "_\\d$")) |>
        dplyr::pull(.data$group_level)

      private$.outcomeNames <- private$.result |>
        dplyr::filter(.data$variable_name == "outcome") |>
        dplyr::distinct(.data$variable_level) |>
        dplyr::pull(.data$variable_level)
    }
  )
)

# Functions ----
#' moduleSurvival
#'
#' Wrapper function to create a CohortSurvival module instance.
#'
#' @param result (`summarised_result`) Result from either `estimateCompetingRiskSurvival()` or `estimateSingleEventSurvival()` functions from the CohortSurvival package.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `TreatmentPatterns` ShinyModule
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleSurvival(tpr)
#' }
moduleSurvival <- function(result, .softValidation) {
  assertType(result, type = c("survival_estimates", "survival_events", "survival_summary"))
  checkCDMNames(result, .softValidation)
  CohortSurvival$new(result)
}

#' shinySurvival
#'
#' @param result (`summarised_result`) Result from either `estimateCompetingRiskSurvival()` or `estimateSingleEventSurvival()` functions from the CohortSurvival package.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#' @returns `shiny.appobj`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinySurvival(result)
#' }
shinySurvival <- function(result, .softValidation) {
  launchBslibApp(
    list(
      Survival = moduleSurvival(result, .softValidation)
    )
  )
}
