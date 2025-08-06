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

  ## Active ----
  active = list(
    #' @field data (`SummarisedResult`) Summarised result object from `CohortSurvival`
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
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
        return(private$.tidyTable)
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

  ## Public ----
  public = list(
    #' @description
    #' Initializer function
    #'
    #' @param data (`SummarisedResults`) Summarised result object from `CohortSurvival`
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `invisible(self)`
    initialize = function(data, ...) {
      checkmate::assertClass(x = data, classes = c("summarised_result", "omop_result"))
      super$initialize(...)
      private$.data <- data
      private$initInputValues()
      private$initPlot()
      private$initTidyTable()
      private$initTable()
      return(invisible(self))
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .data = NULL,
    .plot = NULL,
    .tidyTable = NULL,
    .table = NULL,
    .inputPanel = NULL,

    ### Methods ----
    .UI = function() {
      shiny::wellPanel(
        shiny::column(
          width = 2,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 10,
          private$.plot$UI(),
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Tidy Data",
              private$.tidyTable$UI()
            ),
            shiny::tabPanel(
              title = "Raw Data",
              private$.table$UI()
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.plot$server(input, output, session)
      private$.tidyTable$server(input, output, session)
      private$.table$server(input, output, session)

      private$updatePlotArgs()
    },
    updatePlotArgs = function() {
      shiny::observeEvent(private$.inputPanel$inputValues$plotFacet, {
        private$.plot$args$facet <- private$.inputPanel$inputValues$plotFacet
      })

      shiny::observeEvent(private$.inputPanel$inputValues$plotColour, {
        private$.plot$args$colour <- private$.inputPanel$inputValues$plotColour
      })
    },
    getInputOptions = function() {
      c(
        private$.data %>%
          filter(
            .data$variable_name == "outcome",
            .data$strata_name != "overall",
            !grepl(pattern = "&&&", x = strata_name)
          ) %>%
          pull(.data$strata_name) %>%
          unique(),

        # Additional options
        "target_cohort"
      )
    },
    initInputValues = function() {
      inputOptions <- private$getInputOptions()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          plotFacet = shinyWidgets::pickerInput,
          plotColour = shinyWidgets::pickerInput
        ),
        args = list(
          plotFacet = list(
            inputId = "plotFacet",
            label = "Facet",
            choices = inputOptions,
            multiple = TRUE
          ),
          plotColour = list(
            inputId = "plotColour",
            label = "Colour",
            choices = inputOptions,
            multiple = TRUE
          )
        )
      )
      private$.inputPanel$parentNamespace <- self$namespace
    },
    initPlot = function() {
      args <- if (
        (private$.data %>%
          dplyr::filter(.data$group_name == "target_cohort") %>%
          dplyr::distinct(.data$group_level) %>%
          nrow() / 2) > 1
      ) {
        list(result = private$.data, colour = "target_cohort")
      } else {
        list(result = private$.data)
      }

      private$.plot <- PlotPlotly$new(
        title = NULL,
        fun = CohortSurvival::plotSurvival,
        args = args
      )
      private$.plot$parentNamespace <- self$namespace
    },
    initTidyTable = function() {
      private$.tidyTable <- GTTable$new(
        fun = CohortSurvival::tableSurvival,
        args = list(x = private$.data)
      )
      private$.tidyTable$parentNamespace <- self$namespace
    },
    initTable = function() {
      private$.table <- Table$new(
        title = NULL,
        data = private$.data
      )
      private$.table$parentNamespace <- self$namespace
    }
  )
)
