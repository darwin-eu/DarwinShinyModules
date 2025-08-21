# Copyright 2025 DARWIN EU®
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

#' @title EventDuration
#'
#' @description
#' Module that displays the Event Duration from the `TreatmentPatterns` package.
#'
#' @export
#'
#' @examples{
#'   if (interactive()) {
#'     library(DarwinShinyModules)
#'
#'     tpr <- TreatmentPatterns::TreatmentPatternsResults$new(
#'       filePath = system.file(package = "DarwinShinyModules", "dummyData/TreatmentPatterns/3.0.0/")
#'     )
#'
#'     treatmentPathways <- EventDuration$new(
#'       treatmentPathways = tpr$treatment_pathways,
#'       cdmSourceInfo = tpr$cdm_source_info
#'     )
#'
#'     preview(treatmentPathways)
#'   }
#' }
#'
EventDuration <- R6::R6Class(
  classname = "EventDuration",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field plot (`PlotPlotly`) module.
    plot = function() {
      return(private$.plot)
    },

    #' @field table (`Table`) module.
    table = function() {
      return(private$.table)
    },

    #' @field inputPanel (`InputPanel`) module.
    inputPanel = function() {
      return(private$.inputPanel)
    },

    #' @field summaryEventDuration (`data.table`)
    summaryEventDuration = function(summaryEventDuration) {
      if (missing(summaryEventDuration)) {
        return(private$.summaryEventDuration)
      } else {
        private$.summaryEventDuration <- summaryEventDuration
      }
    },

    #' @field cdmSourceInfo (`data.frame`)
    cdmSourceInfo = function(cdmSourceInfo) {
      if (missing(cdmSourceInfo)) {
        return(private$.cdmSourceInfo)
      } else {
        private$.cdmSourceInfo <- cdmSourceInfo
      }
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param summaryEventDuration (`data.frame`) `summary_event_duration` field from the `TreatmentPatternsResult` object.
    #' @param cdmSourceInfo (`data.frame`) `cdm_source_info` field from the `TreatmentPatternsResult` object.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(summaryEventDuration, cdmSourceInfo, ...) {
      super$initialize(...)

      private$assertInstall("TreatmentPatterns", "3.0.0")

      private$.summaryEventDuration <- summaryEventDuration
      private$.cdmSourceInfo <- cdmSourceInfo

      private$initPlot()
      private$initTable()
      private$initInputPanel()

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Nested Modules ----
    .plot = NULL,
    .table = NULL,
    .inputPanel = NULL,
    .summaryEventDuration = NULL,
    .cdmSourceInfo = NULL,

    ## Overrides ----
    .UI = function() {
      shiny::fluidPage(
        shiny::column(
          width = 2,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 10,
          private$.plot$UI(),
        ),
        private$.table$UI()
      )
    },
    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.plot$server(input, output, session)
      private$.table$server(input, output, session)

      cdmSourceInfo <- renameDatabases(private$.cdmSourceInfo)

      dat <- private$.summaryEventDuration |>
        dplyr::inner_join(
          cdmSourceInfo,
          dplyr::join_by(analysis_id == analysis_id)
        )

      private$updatePickers(cdmSourceInfo, dat, session)

      private$.plot$args$eventDurations <- dat
      shiny::observeEvent(
        list(
          private$.table$bindings$rows_selected,
          private$.inputPanel$inputValues$lines,
          private$.inputPanel$inputValues$database,
          private$.inputPanel$inputValues$overall,
          private$.inputPanel$inputValues$time
        ),
        {
          dat$selected <- "0"
          dat[private$.table$bindings$rows_selected, "selected"] <- "1"
          dat <- dat |>
            dplyr::filter(.data$cdm_source_abbreviation == private$.inputPanel$inputValues$database)

          private$.plot$args$eventDurations <- dat

          private$.table$reactiveValues$data <- dat |>
            dplyr::select(
              "event_name", "duration_min", "duration_q1", "duration_median",
              "duration_q2", "duration_max", "duration_average", "duration_sd",
              "event_count", "line", "analysis_id", "target_cohort_id",
              "target_cohort_name"
            )

          private$.plot$args$eventLines <- c(0, as.numeric(private$.inputPanel$inputValues$lines))
          private$.plot$args$includeOverall <- private$.inputPanel$inputValues$overall
          private$.plot$args$xmin <- private$.inputPanel$inputValues$time[1]
          private$.plot$args$xmax <- private$.inputPanel$inputValues$time[2]
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )
    },

    ## Methods ----
    initPlot = function() {
      plotFun <- function(eventDurations, minCellCount = 1, treatmentGroups = "both", eventLines = c(1), includeOverall = TRUE, xmin, xmax) {
        TreatmentPatterns::plotEventDuration(
          eventDurations = eventDurations,
          minCellCount = minCellCount,
          treatmentGroups = treatmentGroups,
          eventLines = eventLines,
          includeOverall = includeOverall
        ) +
          ggplot2::aes(fill = selected) +
          ggplot2::theme(legend.position = "none") +
          ggplot2::xlim(xmin, xmax)
      }

      private$.plot <- PlotStatic$new(
        fun = plotFun,
        args = list(eventDurations = NULL),
        title = NULL,
        parentNamespace = self$namespace
      )
    },
    initTable = function() {
      private$.table <- Table$new(
        data = NULL,
        title = NULL,
        filter = "none",
        parentNamespace = self$namespace
      )
    },
    initInputPanel = function() {
      databaseLabels <- private$.cdmSourceInfo |>
        dplyr::pull(.data$cdm_source_abbreviation) |>
        unique()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          database = shinyWidgets::pickerInput,
          lines = shinyWidgets::pickerInput,
          overall = shiny::checkboxInput,
          time = shiny::sliderInput
        ),
        args = list(
          database = list(
            inputId = shiny::NS(self$namespace, "database"),
            label = "Database",
            choices = databaseLabels,
            selected = databaseLabels[1],
            multiple = FALSE
          ),
          lines = list(
            inputId = shiny::NS(self$namespace, "lines"),
            label = "Event Lines",
            choices = "1",
            selected = "1",
            multiple = TRUE,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          ),
          overall = list(
            inputId = shiny::NS(self$namespace, "overall"),
            label = "Show over all event lines",
            value = TRUE
          ),
          time = list(
            inputId = "time",
            label = "Time period",
            min = min(private$.summaryEventDuration$duration_min),
            max = max(private$.summaryEventDuration$duration_max),
            value = c(
              min(private$.summaryEventDuration$duration_min),
              max(private$.summaryEventDuration$duration_max)
            )
          )
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace
      )
    },
    updatePickers = function(cdmSourceInfo, dat, session) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(private$.inputPanel$moduleId, "database"),
        choices = cdmSourceInfo$cdm_source_abbreviation
      )

      lines <- dat$line |>
        as.numeric() |>
        unique()

      lines <- lines[!is.na(lines)]

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(private$.inputPanel$moduleId, "lines"),
        choices = lines,
        selected = NULL
      )
    }
  )
)
