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
    #' @field data (`SummarisedResult`) Summarised result object from `CohortSurvival`
    data = function(data) {
      if (missing(data)) {
        return(private$.result)
      }
    },

    #' @field plot (`Plot`) Plot module.
    plot = function(plot) {
      if (missing(plot)) {
        return(private$.plot)
      }
    },

    #' @field tidyTable (`GTTable`) GTTable module
    tidyTable = function(tidyTable) {
      if (missing(tidyTable)) {
        return(private$.survTable)
      }
    },

    #' @field table (`Table`) Table module
    table = function(table) {
      if (missing(table)) {
        return(private$.table)
      }
    },

    #' @field inputPanel (`InputPanel`) InputPanel module
    inputPanel = function(inputPanel) {
      if (missing(inputPanel)) {
        return(private$.inputPanel)
      }
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

      private$initInputValues()
      private$initPlot()
      private$initRiskTable()
      private$initSurvTable()
      private$initRiskTableInput()
      private$initSurvTableInput()
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,
    .plot = NULL,
    .riskTable = NULL,
    .survTable = NULL,
    .table = NULL,
    .inputPanel = NULL,
    .riskInputPanel = NULL,
    .survInputPanel = NULL,

    .cdmName = NULL,
    .cohortNames = NULL,
    .strata = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::h4("General Settings"),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "CDM Name",
            choices = private$.cdmName,
            selected = private$.cdmName[1]
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

        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Risk Table",
            shiny::column(
              width = 2,
              shiny::h4("Settings"),
              shinyWidgets::pickerInput(inputId = "dummy-a", label = "Dummy", choices = c("foo", "bar"))
            ),
            shiny::column(
              width = 10,
              private$.riskTable$UI()
            )
          ),

          shiny::tabPanel(
            title = "Survival Table",
            shiny::column(
              width = 2,
              shinyWidgets::pickerInput(inputId = "dummy-b", label = "Dummy", choices = c("foo", "bar"))
            ),
            shiny::column(
              width = 10,
              private$.survTable$UI()
            )
          ),

          shiny::tabPanel(
            title = "Kaplan-Meier Plot",
            shiny::column(
              width = 2,
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "plotRibbon"),
                label = "Ribbon",
                choices = c("On", "off"),
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
        )
      )
    },

    .uiGeneralFilters = function() {},

    .uiPlot = function() {},

    .uiTableSurv = function() {},

    .uiTableRisk = function() {},

    ## Server ----
    .server = function(input, output, session) {
      private$.serverPlot(input, output, session)
      # private$.inputPanel$server(input, output, session)
      # private$.riskInputPanel$server(input, output, session)
      # private$.survInputPanel$server(input, output, session)
      # private$.table$server(input, output, session)

      # private$.serverPlot()
      # private$.serverTableSurv()
      # private$.serverTableRisk()
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

    .serverTableSurv = function() {
      shiny::observeEvent(
        list(
          private$.survInputPanel$inputValues$survTimeScale,
          private$.survInputPanel$inputValues$survTimes,
          private$.survInputPanel$inputValues$survHeader,
          private$.survInputPanel$inputValues$survGroupColumn
        ), {
          private$.survTable$args$timeScale <- private$.survInputPanel$inputValues$survTimeScale

          survTimes <- private$.survInputPanel$inputValues$survTimes |>
            strsplit(split = ",") |>
            unlist() |>
            as.numeric()

          if (!is.numeric(survTimes) | length(survTimes) == 0) {
            survTimes <- NULL
          }

          private$.survTable$args$times <- survTimes
          private$.survTable$args$header <- private$.survInputPanel$inputValues$survHeader
          private$.survTable$args$groupColumn <- private$.survInputPanel$inputValues$survGroupColumn
          private$.survTable$server(input, output, session)
        }
      )
    },

    .serverTableRisk = function() {
      shiny::observeEvent(
        list(
          private$.riskInputPanel$inputValues$riskHeader,
          private$.riskInputPanel$inputValues$riskGroupColumn
        ), {
          private$.riskTable$args$header <- private$.riskInputPanel$inputValues$riskHeader
          private$.riskTable$args$groupColumn <- private$.riskInputPanel$inputValues$riskGroupColumn
          private$.riskTable$server(input, output, session)
        }
      )
    },

    # Initializers ----
    initInputValues = function() {
      inputOptions <- private$getInputOptions()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          plotFacet = shinyWidgets::pickerInput,
          plotColour = shinyWidgets::pickerInput,
          plotRibbon = shinyWidgets::switchInput,
          plotCumFail = shinyWidgets::switchInput
        ),
        args = list(
          plotFacetX = list(
            inputId = "plotFacetX",
            label = "Horizontal Facet",
            choices = inputOptions,
            multiple = TRUE
          ),
          plotFacetY = list(
            inputId = "plotFacetY",
            label = "Vertical Facet",
            choices = inputOptions,
            multiple = TRUE
          ),
          plotColour = list(
            inputId = "plotColour",
            label = "Colour",
            choices = inputOptions,
            multiple = TRUE
          ),
          plotRibbon = list(
            inputId = "Ribbon",
            label = "",
            onLabel = "Ribbon",
            offLabel = "No Ribon",
            labelWidth = 0,
            size = "mini",
            value = TRUE
          ),
          plotCumFail = list(
            inputId = "plotCumFail",
            label = "",
            size = "mini",
            labelWidth = 0,
            onLabel = "Plot Failure",
            offLabel = "Plot Survival",
            value = FALSE
          )
        ),
        parentNamespace = self$namespace
      )
    },

    initRiskTableInput = function() {
      selectOptions <- c("estimate", "cdm_name", private$fetchStrata(), "settings", "time", "overall")

      private$.riskInputPanel <- InputPanel$new(
        funs = list(
          riskHeader = shinyWidgets::pickerInput,
          riskGroupColumn = shinyWidgets::pickerInput
        ),
        args = list(
          riskHeader = list(
            inputId = "riskHeader",
            label = "Headers",
            choices = selectOptions,
            multiple = TRUE
          ),
          riskGroupColumn = list(
            inputId = "riskGroupColumn",
            label = "Group Column",
            choices = selectOptions,
            multiple = TRUE
          )
        ),
        parentNamespace = self$namespace
      )
    },

    initSurvTableInput = function() {
      selectOptions <- c("estimate", "cdm_name", private$fetchStrata(), "settings", "time", "overall")

      private$.survInputPanel <- InputPanel$new(
        funs = list(
          survTimeScale = shinyWidgets::pickerInput,
          survTimes = shiny::textInput,
          survHeader = shinyWidgets::pickerInput,
          survGroupColumn = shinyWidgets::pickerInput
        ),
        args = list(
          survTimeScale = list(
            inputId = "survTimeScale",
            label = "Time Scale",
            choices = c("days", "months", "years"),
            selected = "days"
          ),
          survTimes = list(
            inputId = "survTimes",
            label = "Times (multiple split by: ',' like: 1,2,3)"
          ),
          survHeader = list(
            inputId = "survHeader",
            label = "Headers",
            choices = selectOptions,
            multiple = TRUE
          ),
          survGroupColumn = list(
            inputId = "survGroupColumn",
            label = "Group Column",
            choices = selectOptions,
            multiple = TRUE
          )
        ),
        parentNamespace = self$namespace
      )
    },

    initPlot = function() {
      private$.plot <- PlotStatic$new(
        title = NULL,
        fun = CohortSurvival::plotSurvival,
        args = list(style = "darwin"),
        height = "80vh",
        parentNamespace = self$namespace
      )
    },
    initRiskTable = function() {
      private$.riskTable <- GTTable$new(
        fun = CohortSurvival::riskTable,
        args = list(x = private$.result, .options = list(style = "darwin")),
        parentNamespace = self$namespace
      )
    },

    initSurvTable = function() {
      private$.survTable <- GTTable$new(
        fun = CohortSurvival::tableSurvival,
        args = list(x = private$.result, .options = list(style = "darwin")),
        parentNamespace = self$namespace
      )
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
        dplyr::filter(.data$strata_name != "overall") |>
        dplyr::pull(.data$strata_name)
    },

    fetchStrata = function() {
      private$.result %>%
        dplyr::distinct(.data$strata_name) %>%
        dplyr::filter(
          !.data$strata_name %in% c("overall", "reason"),
          !stringr::str_detect(.data$strata_name, " &&& ")
        ) %>%
        dplyr::pull()
    },

    getInputOptions = function() {
      c(
        private$.result %>%
          dplyr::filter(
            .data$variable_name == "outcome",
            .data$strata_name != "overall",
            !grepl(pattern = "&&&", x = strata_name)
          ) %>%
          dplyr::pull(.data$strata_name) %>%
          unique(),

        # Additional options
        "target_cohort"
      )
    }
  )
)
