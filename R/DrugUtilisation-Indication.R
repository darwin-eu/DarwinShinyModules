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

#' @title Indication Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Indication module that shows a that supports results from the
#' `summariseIndication()` function from the DrugUtilisation package.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")
#'
#'   cdm <- DrugUtilisation::generateIngredientCohortSet(
#'     cdm = cdm,
#'     name = "dus_cohort",
#'     ingredient = "acetaminophen",
#'     gapEra = 7
#'   )
#'
#'   indications <- list(headache = 378253, influenza = 4266367)
#'
#'   cdm <- CDMConnector::generateConceptCohortSet(
#'     cdm = cdm,
#'     conceptSet = indications,
#'     name = "indications_cohort"
#'   )
#'
#'   result <- cdm$dus_cohort |>
#'     PatientProfiles::addAge(
#'       ageGroup = list(
#'         `0-17` = c(0, 17),
#'         `>=18` = c(18, Inf)
#'       )
#'     ) |>
#'     PatientProfiles::addSex() |>
#'     DrugUtilisation::summariseIndication(
#'       indicationCohortName = "indications_cohort",
#'       unknownIndicationTable = "condition_occurrence",
#'       indicationWindow = list(c(-30, 0)),
#'       strata = list(
#'         "age_group",
#'         "sex"
#'       )
#'     )
#'
#'   mod <- Indication$new(result)
#'
#'   DarwinShinyModules::preview(mod)
#' }
Indication <- R6::R6Class(
  classname = "Indication",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`sumamrised_result`)
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

      private$.table <- DarwinShinyModules::Flextable$new(
        fun = DrugUtilisation::tableIndication,
        args = list(result = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- DarwinShinyModules::PlotStatic$new(
        fun = private$.plotFun,
        args = list(result = private$.result, style = "darwin"),
        title = NULL,
        height = "80vh",
        parentNamespace = self$namespace
      )
      return(self)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,

    .table = NULL,
    .plot = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,
    .strata = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        private$.uiGeneralSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            shiny::column(
              width = 2,
              private$.uiTableSettings()
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
              private$.uiPlotSettings()
            ),
            shiny::column(
              width = 10,
              private$.plot$UI()
            )
          )
        )
      )
    },

    .uiGeneralSettings = function() {
      shiny::fluidPage(
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

    .uiTableSettings = function() {
      shiny::fluidPage(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "strata"),
          label = "Strata",
          choices = private$.strata,
          multiple = TRUE,
          options = private$.pickerOptions
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "header"),
          label = "Header",
          choices = availableTableColumns(private$.result),
          multiple = TRUE,
          options = private$.pickerOptions
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "groupColumn"),
          label = "Group Column",
          choices = availableTableColumns(private$.result),
          multiple = TRUE,
          options = private$.pickerOptions
        )
      )
    },

    .uiPlotSettings = function() {
      shiny::fluidPage(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "position"),
          label = "Bar position",
          choices = c("stack", "dodge"),
          selected = "stack"
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetX"),
          label = "Horizontal Facet",
          choices = availablePlotColumns(private$.result),
          multiple = TRUE,
          options = private$.pickerOptions
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facetY"),
          label = "Vertical Facet",
          choices = availablePlotColumns(private$.result),
          multiple = TRUE,
          options = private$.pickerOptions
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "colour"),
          label = "Colour",
          choices = availablePlotColumns(private$.result),
          multiple = TRUE,
          options = private$.pickerOptions
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.serverTable(input, output, session)
      private$.serverPlot(input, output, session)
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName,
        input$cohortName,
        input$header,
        input$groupColumn,
        input$strata
      ), {
        strata <- if (all(is.null(input$strata))) {
          "overall"
        } else {
          c("overall", input$strata)
        }

        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$strata_name %in% strata
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn
        private$.table$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName,
        input$cohortName,
        input$position,
        input$facetX,
        input$facetY,
        input$colour
      ), {
        private$.plot$args$result <- private$.result |>
          dplyr::filter(.data$cdm_name %in% input$cdmName) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.plot$args$position <- input$position
        private$.plot$args$facet <- makeFacetFormula(input$facetX, input$facetY)
        private$.plot$args$colour <- input$colour
        private$.plot$server(input, output, session)
      })
    },

    ## Helpers ----
    .setFilterValues = function() {
      private$.cdmNames <- getCDMNames(private$.result)
      private$.cohortNames <- getCohortNames(private$.result)
      private$.strata <- getStrata(private$.result)
    },

    .plotFun = function(...) {
      DrugUtilisation::plotIndication(...) +
        ggThemeDarwin()
    }
  )
)

# Functions ----
#' moduleIndication
#'
#' @param result (`summarised_result`) Result from the `summariseIndication` function from the DrugUtilisation pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleIndication(result)
#' }
moduleIndication <- function(result, .softValidation = FALSE) {
  assertType(result, "summarise_indication")
  checkCDMNames(result, .softValidation)
  Indication$new(result)
}

#' shinyIndication
#'
#' @param result (`summarised_result`) Result from the `summariseIndication` function from the DrugUtilisation pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinyIndication(result)
#' }
shinyIndication <- function(result, .softValidation = FALSE) {
  launchBslibApp(
    list(
      Indication = moduleIndication(result, .softValidation)
    )
  )
}
