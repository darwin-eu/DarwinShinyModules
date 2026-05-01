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

#' @title DrugUtilisation Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' DrugUtilisation module that shows a that supports results from the
#' `summariseDrugutilisation()` function from the DrugUtilisation package.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "DrugUtilisation",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")
#'
#'     cdm <- DrugUtilisation::generateIngredientCohortSet(
#'       cdm = cdm,
#'       name = "dus_cohort",
#'       ingredient = "acetaminophen",
#'       gapEra = 7
#'     )
#'
#'     result <- cdm$dus_cohort |>
#'       PatientProfiles::addAge(ageGroup = list(
#'         `0-17` = c(0, 17),
#'         `>=18` = c(18, Inf)
#'       )) |>
#'       PatientProfiles::addSex() |>
#'       DrugUtilisation::summariseDrugUtilisation(
#'         ingredientConceptId = 1125315,
#'         gapEra = 7,
#'         strata = list("age_group", "sex")
#'       )
#'
#'     duMod <- DrugUtilisation$new(result = result)
#'     preview(duMod)
#'   }
#' }
DrugUtilisation <- R6::R6Class(
  classname = "DrugUtilisation",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`)
    result = function() {
      return(private$.result)
    },

    #' @field table (`Flextable`)
    table = function() {
      return(private$.table)
    },

    #' @field plot (`PlotStatic`)
    plot = function() {
      return(private$.plot)
    },

    #' @field cdmNames (`character(n)`)
    cdmNames = function() {
      return(private$.cdmNames)
    },

    #' @field cohortNames (`character(n)`)
    cohortNames = function() {
      return(private$.cohortNames)
    },

    #' @field strata (`character(n)`)
    strata = function() {
      return(private$.strata)
    },

    #' @field variables (`character(n)`)
    variables = function() {
      return(private$.variables)
    },

    #' @field estimates (`character(n)`)
    estimates = function() {
      return(private$.estimates)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method.
    #'
    #' @param result (`summarised_result`) Object created by `DrugUtilisation::summariseDrugUtilisation()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      super$initialize(...)
      private$.result <- result

      private$.setFilterValues()

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableDrugUtilisation,
        args = list(style = "darwin", type = "flextable"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = DrugUtilisation::plotDrugUtilisation,
        args = list(style = "darwin"),
        height = "80vh",
        parentNamespace = self$namespace
      )
      return(self)
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
    .variables = NULL,
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
      shiny::fluidPage(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "CDM Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
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
          choices = private$.variables,
          selected = private$.variables[1]
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "estimates"),
          label = "Estimates",
          choices = private$.estimates,
          selected = private$.estimates[1],
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "plotType"),
          label = "Plot Type",
          choices = c(),
          selected = "barplot"
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetX"),
          label = "Horizontal Facet",
          choices = c("cdm_name", "cohort_name", private$.strata),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetY"),
          label = "Vertical Facet",
          choices = c("cdm_name", "cohort_name", private$.strata),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "colour"),
          label = "Colour",
          choices = c("cdm_name", "cohort_name", private$.strata),
          multiple = TRUE
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.updateEstimates(input, output, session)
      private$.updatePlotType(input, output, session)
      private$.serverTable(input, output, session)
      private$.serverPlot(input, output, session)
    },

    .updateEstimates = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName, input$cohortName
      ), {
        res <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        estimates <- res |>
          dplyr::distinct(.data$estimate_name) |>
          dplyr::filter(!.data$estimate_name %in% c("count")) |>
          dplyr::pull(.data$estimate_name)

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "estimates",
          choices = estimates,
          selected = estimates[1]
        )
      })
    },

    .updatePlotType = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName, input$cohortName, input$estimates
      ), {
        res <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$estimate_name %in% input$estimates
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        estimates <- res |>
          dplyr::distinct(.data$estimate_name) |>
          dplyr::pull(.data$estimate_name)

        if (all(c("min", "q25", "median", "q75", "max") %in% estimates)) {
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "plotType",
            choices = c("boxplot", "scatterplot", "barplot"),
            selected = "boxplot"
          )
        } else {
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "plotType",
            choices = c("barplot", "scatterplot"),
            selected = "barplot"
          )
        }
      })
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
        inputStrata <- unique(c(input$facetX, input$facetY, input$colour))

        strata <- if (all(is.null(inputStrata))) {
          "overall"
        } else {
          c("overall", inputStrata)
        }

        private$.plot$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$estimate_name %in% input$estimates,
            .data$strata_name %in% strata
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

      private$.estimates <- private$.result |>
        dplyr::distinct(.data$estimate_name) |>
        dplyr::filter(!.data$estimate_name %in% c("count")) |>
        dplyr::pull(.data$estimate_name)

      private$.variables <- private$.result |>
        dplyr::distinct(.data$variable_name) |>
        dplyr::filter(!.data$variable_name %in% c("number records", "number subjects")) |>
        dplyr::pull(.data$variable_name)
    }
  )
)
