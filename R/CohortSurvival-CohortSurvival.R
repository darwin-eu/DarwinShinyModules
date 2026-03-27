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
#'     library(CDMConnector)
#'     library(CohortSurvival)
#'
#'     cdm <- CohortSurvival::mockMGUS2cdm()
#'
#'     MGUS_death <- estimateSingleEventSurvival(
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
#'     cs <- CohortSurvival$new(data = MGUS_death)
#'     if (interactive()) {
#'       preview(cs)
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

    #' @field plot (`Plot`) Plot module. using `CohortSurvival::plotSurvival()`
    plot = function() {
      return(private$.plot)
    },

    #' @field table (`Table`) Table module using `CohortSurvival::tableSurvival()`
    table = function() {
        return(code)
    },

    #' @field tableEvents (`Table`) Table module using `CohortSurvival::tableSurvivalEvents()`
    tableEvents = function() {
      return(private$.tableEvents)
    },

    #' @field tableAttrition (`Table`) Table module using `CohortSurvival::tableAttrition()`
    tableAttrition = function() {
      return(private$.tableAttrition)
    },

    #' @field cdmName (`character`) Available CDM names
    cdmName = function() {
      return(private$.cdmName)
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

      private$.plot <- PlotStatic$new(
        title = NULL,
        fun = CohortSurvival::plotSurvival,
        args = list(style = "darwin"),
        height = "80vh",
        parentNamespace = self$namespace
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

    .table = NULL,
    .tableEvents = NULL,
    .plot = NULL,
    .tableAttrition = NULL,

    .cdmName = NULL,
    .cohortNames = NULL,
    .strata = NULL,

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
      shiny::tagList(
        shiny::h4("General Settings"),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "CDM Name",
            choices = private$.cdmName,
            selected = private$.cdmName[1],
            multiple = TRUE
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "strata"),
            label = "Strata",
            choices = private$.strata,
            selected = "overall",
            multiple = TRUE
          )
        )
      )
    },

    .uiTable = function() {
      shiny::tagList(
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
            multiple = TRUE
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

      shiny::tagList(
        shiny::column(
          width = 2,
          shiny::h4("Settings"),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTableSurvEventGap"),
            label = "eventGap",
            choices = eventGap,
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 10,
          private$.tableEvents$UI()
        )
      )
    },

    .uiPlot = function() {
      shiny::tagList(
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
            choices = c("cdm_name", "target_cohort", private$.strata),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetY"),
            label = "Vertical facet",
            choices = c("cdm_name", "target_cohort", private$.strata),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotColour"),
            label = "Colour",
            choices = c("cdm_name", "target_cohort", private$.strata),
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 10,
          private$.plot$UI()
        )
      )
    },

    .uiTableAttrition = function() {
      private$.tableAttrition$UI()
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
          input$cdm_name, input$cohortName, input$strata,
          input$tableTimeScale, input$tableTimes, input$tableEstimates
        ), {
          private$.table$args$timeScale <- input$tableTimeScale
          private$.table$args$times <- as.numeric(input$tableTimes)
          private$.table$args$estimates <- input$tableEstimates

          private$.table$args$x <- private$.result |>
            dplyr::filter(
              .data$cdm_name %in% input$cdmName,
              .data$strata_name %in% input$strata
            ) |>
            omopgenerics::filterGroup(target_cohort %in% input$cohortName)

          private$.table$server(input, output, session)
        }
      )
    },

    .serverTableEvents = function(input, output, session) {
      shiny::observeEvent(list(input$cdmName, input$cohortName, input$strata), {
        private$.tableEvents$args$x <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$strata_name %in% input$strata
          ) |>
          omopgenerics::filterGroup(.data$target_cohort %in% input$cohortName)

        private$.tableEvents$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(
        list(
          input$cdmName,
          input$cohortName,
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
              .data$cdm_name %in% input$cdmName,
              .data$strata_name %in% strata
            ) |>
            omopgenerics::filterGroup(target_cohort %in% input$cohortName)

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
      private$.cdmName <- private$.result |>
        dplyr::distinct(.data$cdm_name) |>
        dplyr::pull(.data$cdm_name)

      private$.cohortNames <- private$.result |>
        dplyr::filter(
          .data$group_name == "target_cohort",
          .data$strata_name != "reason"
        ) |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)

      private$.strata <- private$.result |>
        dplyr::filter(.data$strata_name != "reason") |>
        dplyr::distinct(.data$strata_name) |>
        dplyr::pull(.data$strata_name)
    }
  )
)
