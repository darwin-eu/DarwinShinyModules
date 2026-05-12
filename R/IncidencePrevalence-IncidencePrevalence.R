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

#' @title IncidencePrevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' IncidencePrevalence module that shows incidence results from the IncidencePrevalence package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "IncidencePrevalence",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     inc <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/IncidencePrevalence/1.2.0/incidence.csv"
#'     ))
#'
#'     incMod <- IncidencePrevalence$new(result = inc,
#'                             defaults = list(sex = "Both"))
#'
#'     ui <- shiny::fluidPage(
#'       incMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       incMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarisedResult`) SummarisedResult object from IncidencePrevalence
    result = function(result) {
      if (missing(result)) {
        return(private$.result)
      } else {
        # Checks on result
        checkmate::assertClass(result, "summarised_result")
        private$.result <- result
      }
    },

    #' @field pickers (`list`) List of pickers
    pickers = function() {
      return(private$.pickers)
    }
  ),

  # Public ----
  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param result (`summarised_result`) Result object from the `IncidencePrevalence` package.
    #' @param defaults list of default values for the pickers
    #' @param type (`character(1)`) Either `"incidence"` or `"prevalence"`
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, defaults = list(), ...) {
      super$initialize(...)
      private$.assertResult(result)
      private$.result <- result
      private$.settings <- omopgenerics::settings(result)
      private$.defaults <- defaults

      resType <- unique(private$.settings$result_type)

      if ("incidence" %in% resType) {
        private$.setIncidenceCols()
      } else if ("prevalence" %in% resType) {
        private$.setPrevalenceCols()
      }

      private$.setFilterValues()
      private$.initPickers()

      private$.table <- Flextable$new(
        fun = private$.tableIncidencePrevalence,
        args = list(
          type = "flextable",
          style = "darwin"
        ),
        height = "80vh",
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = private$.plotIncidencePrevalence,
        args = list(),
        height = "80vh",
        parentNamespace = self$namespace
      )

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .resultType = "",
    .startDateCol = "",
    .endDateCol = "",
    .estimate = "",
    .ciUpper = "",
    .ciLower = "",

    .result = NULL,
    .settings = NULL,

    .pickers = NULL,
    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .strata = NULL,
    .defaults = NULL,

    .table = NULL,
    .plot = NULL,

    .cdmNames = NULL,
    .denominatorCohorts = NULL,
    .outcomeCohorts = NULL,

    .ageGroups = NULL,
    .sex = NULL,
    .priorObs = NULL,
    .startDate = NULL,
    .endDate = NULL,
    .timeAtRisk = NULL,
    .timeIntervals = NULL,

    # Incidence
    .outcomeWashout = NULL,
    .repeatedEvents = NULL,
    .databaseIntervals = NULL,

    # Point Prev
    .timePoint = NULL,

    # Period Prev
    .fullContribution = NULL,
    .level = NULL,

    .dateRangeMin = NULL,
    .dateRangeMax = NULL,

    .hasInterval = FALSE,

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        private$.uiInputs(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Plot",
            private$.uiPlot()
          ),
          shiny::tabPanel(
            title = "Table",
            private$.uiTable()
          )
        )
      )
    },

    .uiInputs = function() {
      label <- if (private$.resultType == "incidence") {
        "Incidence"
      } else if (private$.resultType == "point_prev") {
        "Point Prevalence"
      } else if (private$.resultType == "period_prev") {
        "Period Prevalence"
      }

      shiny::fluidRow(
        shiny::h3(sprintf("%s Estimates", label)),
        shiny::p(sprintf("%s Estimates are shown below, please select configuration to filter them:", label)),
        shiny::h4("Settings"),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Database, study outcome and strata",
            private$.pickers[["database"]]$UI()
          ),
          shiny::tabPanel(
            title = "Denominator Population",
            private$.pickers[["denominator"]]$UI()
          ),
          shiny::tabPanel(
            title = "Analysis",
            private$.pickers[["analysis"]]$UI()
          ),
          shiny::tabPanel(
            title = "Dates",
            private$.pickers[["date"]]$UI()
          )
        )
      )
    },

    .uiPlot = function() {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 2,
            private$.pickers[["plot"]]$UI()
          ),
          shiny::column(
            width = 10,
            private$.plot$UI()
          )
        )
      )
    },

    .uiTable = function() {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 2,
            private$.pickers[["table"]]$UI()
          ),
          shiny::column(
            width = 10,
            private$.table$UI()
          )
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      private$.serverUpdateIntervalItems(input, output, session)

      summariseResultData <- private$.serverGetSummariseResultData(input, output, session)

      private$.serverPlot(input, output, session, fetchData = summariseResultData)
      private$.serverTable(input, output, session, fetchData = summariseResultData)
    },

    .serverUpdateIntervalItems = function(input, output, session) {
      # "overall", "years", "quarters", "months", "weeks"
      shiny::observeEvent(list(
        private$.pickers[["date"]]$inputValues$interval,
        private$.pickers[["date"]]$inputValues$timeWindow
      ), {
        interval <- private$.pickers[["date"]]$inputValues$interval
        minDate <- private$.pickers[["date"]]$inputValues$timeWindow[1]
        maxDate <- private$.pickers[["date"]]$inputValues$timeWindow[2]

        if (private$.hasInterval) {
          additionalLevels <- private$.result |>
            omopgenerics::filterAdditional(.data[[private$.endDateCol]] != "overall") |>
            omopgenerics::filterAdditional(as.Date(.data[[private$.startDateCol]]) >= minDate) |>
            omopgenerics::filterAdditional(as.Date(.data[[private$.endDateCol]]) <= maxDate) |>
            dplyr::filter(grepl(pattern = "analysis_interval", x = .data$additional_name)) |>
            dplyr::filter(grepl(pattern = interval, x = .data$additional_level)) |>
            dplyr::distinct(.data$additional_level) |>
            dplyr::pull(.data$additional_level)

          additionalLevelItems <- stringr::str_split_i(string = additionalLevels, pattern = " &&& ", i = 1) |>
            unlist() |>
            unique()

          private$.pickers[["date"]]$update(
            fun = shinyWidgets::updatePickerInput,
            name = "intervalItems",
            session = session,
            choices = additionalLevelItems,
            selected = additionalLevelItems[1]
          )
        } else {
          private$.pickers[["date"]]$update(
            fun = shinyWidgets::updatePickerInput,
            name = "intervalItems",
            session = session,
            choices = "overall",
            selected = "overall"
          )
        }
      })
    },

    .serverGetSummariseResultData = function(input, output, session) {
      reactive({
        analysis <- private$.pickers$analysis$inputValues
        denominator <- private$.pickers$denominator$inputValues
        date <- private$.pickers$date$inputValues
        database <- private$.pickers[["database"]]$inputValues

        result <- private$.result |>
          dplyr::filter(
            cdm_name %in% private$.pickers[["database"]]$inputValues$cdm
          )

        result <- if (private$.resultType == "incidence") {
          result |>
            omopgenerics::filterSettings(
              .data$analysis_outcome_washout %in% analysis$washout,
              .data$analysis_repeated_events %in% analysis$repeated_events,
              .data$analysis_complete_database_intervals %in% analysis$complete_period
            )
        } else if (private$.resultType == "point_prev") {
          result
        } else if (private$.resultType == "period_prev") {
          result |>
            omopgenerics::filterSettings(
              .data$analysis_full_contribution %in% analysis$fullContribution,
              .data$analysis_level %in% analysis$level,
              .data$analysis_complete_database_intervals %in% analysis$complete_period
            )
        } else {
          result
        }

        result <- result |>
          omopgenerics::filterSettings(
            .data$denominator_start_date %in% denominator$start_date,
            .data$denominator_end_date %in% denominator$end_date,
            .data$denominator_days_prior_observation %in% denominator$prior_obs,
            .data$denominator_sex %in% denominator$denom_sex,
            .data$denominator_age_group %in% denominator$age_group,
            .data$denominator_time_at_risk %in% denominator$time_at_risk
          )

        if (private$.hasInterval) {
          result <- result |>
            omopgenerics::filterAdditional(
              analysis_interval == date$interval,
              .data[[private$.startDateCol]] %in% date$intervalItems
            )
        }

        result |>
          omopgenerics::filterGroup(outcome_cohort_name %in% database$outcome)
      })
    },

    .serverTable = function(input, output, session, fetchData) {
      inputValues <- private$.pickers[["table"]]$inputValues

      shiny::observe({
        shiny::req(fetchData())
        private$.table$args$result <- fetchData()
        private$.table$args$header <- inputValues$headerColumn
        private$.table$args$groupColumn <- inputValues$groupColumn
        private$.table$args$settingsColumn <- inputValues$settingsColumn
        private$.table$args$hide <- inputValues$hideColumn

        private$.table$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session, fetchData) {
      shiny::observe({
        shiny::req(fetchData())
        private$.plot$args$result <- fetchData()
        private$.plot$args$x <- private$.pickers[["plot"]]$inputValues$xAxis
        private$.plot$args$y <- private$.estimate
        private$.plot$args$line <- FALSE
        private$.plot$args$point <- TRUE
        private$.plot$args$ribbon <- as.logical(private$.pickers[["plot"]]$inputValues$ribbon)
        private$.plot$args$ymin <- private$.ciLower
        private$.plot$args$ymax <- private$.ciUpper
        private$.plot$args$facet <- makeFacetFormula(
          facetX = private$.pickers[["plot"]]$inputValues$facetX,
          facetY = private$.pickers[["plot"]]$inputValues$facetY
        )
        private$.plot$args$colour <- private$.pickers[["plot"]]$inputValues$color_by

        private$.plot$args$confInterval <- as.logical(private$.pickers[["plot"]]$inputValues$confInterval)
        private$.plot$args$line <- as.logical(private$.pickers[["plot"]]$inputValues$line)

        private$.plot$server(input, output, session)
      })
    },

    ## Initializers ----
    .initPickers = function() {
      private$.initDatabaseInputs()
      private$.initDenominatorInputs()
      private$.initAnalysisInputs()
      private$.initDateInputs()
      private$.initPlotInputs()
      private$.initTableInputs()
    },

    .initDatabaseInputs = function() {
      private$.pickers[["database"]] <- InputPanel$new(
        funs = list(
          cdm = shinyWidgets::pickerInput,
          outcome = shinyWidgets::pickerInput,
          strata = shinyWidgets::pickerInput
        ),
        args = list(
          cdm = list(
            inputId = "cdm",
            label = "Data Source",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          outcome = list(
            inputId = "outcome",
            label = "Outcome",
            choices = private$.outcomeCohorts,
            selected = private$.outcomeCohorts[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          strata = list(
            inputId = "strata",
            label = "Strata",
            choices = private$.strata,
            selected = private$.strata[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initDenominatorInputs = function() {
      private$.pickers[["denominator"]] <- InputPanel$new(
        funs = list(
          age_group = shinyWidgets::pickerInput,
          denom_sex = shinyWidgets::pickerInput,
          prior_obs = shinyWidgets::pickerInput,
          start_date = shinyWidgets::pickerInput,
          end_date = shinyWidgets::pickerInput,
          time_at_risk = shinyWidgets::pickerInput
        ),
        args = list(
          age_group = list(
            inputId = "age_group",
            label = "Age group",
            choices = private$.ageGroups,
            selected = private$.ageGroups[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          denom_sex = list(
            inputId = "denom_sex",
            label = "Sex",
            choices = private$.sex,
            selected = private$.sex[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          prior_obs = list(
            inputId = "prior_obs",
            label = "Prior observation",
            choices = private$.priorObs,
            selected = private$.priorObs[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          start_date = list(
            inputId = "start_date",
            label = "Start date",
            choices = private$.startDate,
            selected = private$.startDate[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          end_date = list(
            inputId = "end_date",
            label = "End date",
            choices = private$.endDate,
            selected = private$.endDate[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          time_at_risk = list(
            inputId = "time_at_risk",
            label = "Time at risk",
            choices = private$.timeAtRisk,
            selected = private$.timeAtRisk[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initAnalysisInputs = function() {
      private$.pickers[["analysis"]] <- if (private$.resultType == "incidence") {
        InputPanel$new(
          funs = list(
            washout = shinyWidgets::pickerInput,
            repeated_events = shinyWidgets::pickerInput,
            complete_period = shinyWidgets::pickerInput
          ),
          args = list(
            washout = list(
              inputId = "washout",
              label = "Outcome washout",
              choices = private$.outcomeWashout,
              selected = private$.outcomeWashout[1],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            repeated_events = list(
              inputId = "repeated_events",
              label = "Repeated events",
              choices = private$.repeatedEvents,
              selected = private$.repeatedEvents[1],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            complete_period = list(
              inputId = "complete_period",
              label = "Complete period",
              choices = private$.databaseIntervals,
              selected = private$.databaseIntervals[1],
              multiple = TRUE,
              options = private$.pickerOptions
            )
          ),
          growDirection = "horizontal",
          parentNamespace = self$namespace
        )
      } else if (private$.resultType == "point_prev") {
        # Empty module, no analysis settings are present for point prevalence.
        InputPanel$new(
          funs = list(),
          args = list(),
          growDirection = "horizontal",
          parentNamespace = self$namespace
        )
      } else if (private$.resultType == "period_prev") {
        InputPanel$new(
          funs = list(
            fullContribution = shinyWidgets::pickerInput,
            level = shinyWidgets::pickerInput,
            complete_period = shinyWidgets::pickerInput
          ),
          args = list(
            fullContribution = list(
              inputId = "fullContribution",
              label = "Full Contribution",
              choices = private$.fullContribution,
              selected = private$.fullContribution[1],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            level = list(
              inputId = "level",
              label = "Level",
              choices = private$.level,
              selected = private$.level[1],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            complete_period = list(
              inputId = "complete_period",
              label = "Complete period",
              choices = private$.databaseIntervals,
              selected = private$.databaseIntervals[1],
              multiple = TRUE,
              options = private$.pickerOptions
            )
          ),
          growDirection = "horizontal",
          parentNamespace = self$namespace
        )
      } else {
        InputPanel$new(
          funs = list(),
          args = list(),
          growDirection = "horizontal",
          parentNamespace = self$namespace
        )
      }
    },

    .initDateInputs = function() {
      private$.pickers[["date"]] <- InputPanel$new(
        funs = list(
          interval = shinyWidgets::pickerInput,
          intervalItems = shinyWidgets::pickerInput,
          timeWindow = shiny::dateRangeInput
        ),
        args = list(
          interval = list(
            inputId = "interval",
            label = "Interval Period",
            choices = private$.timeIntervals,
            selected = private$.timeIntervals[1],
            options = private$.pickerOptions
          ),
          intervalItems = list(
            inputId = "intervalItems",
            label = "Intervals",
            choices = c(),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          timeWindow = list(
            inputId = "timeWindow",
            label = "Time Window",
            start = private$.dateRangeMin,
            end = private$.dateRangeMax,
            min = private$.dateRangeMin,
            max = private$.dateRangeMax
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initPlotInputs = function() {
      # plot pickers
      plotDataChoices <- c(
        "cdm_name", "outcome_cohort_name", "strata", "denominator_cohort_name",
        "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk",
        "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_interval", private$.startDateCol
      )

      private$.pickers[["plot"]] <- InputPanel$new(
        funs = list(
          xAxis = shinyWidgets::pickerInput,
          facetX = shinyWidgets::pickerInput,
          facetY = shinyWidgets::pickerInput,
          color_by = shinyWidgets::pickerInput,
          ribbon = shinyWidgets::pickerInput,
          confInterval = shinyWidgets::pickerInput,
          rotateXLabels = shinyWidgets::pickerInput,
          line = shiny::checkboxInput
        ),
        args = list(
          xAxis = list(
            inputId = "xAxis",
            label = "X-Axis",
            choices = plotDataChoices,
            selected = private$.startDateCol,
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          facetX = list(
            inputId = "facetX",
            label = "Horizontal Facet",
            choices = plotDataChoices,
            selected = c("outcome_cohort_name"),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          facetY = list(
            inputId = "facetY",
            label = "Vertical Facet",
            choices = plotDataChoices,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          color_by = list(
            inputId = "color_by",
            label = "Colour by",
            choices = plotDataChoices,
            selected = c(),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          ribbon = list(
            inputId = "ribbon",
            label = "Ribbon",
            choices = c(TRUE, FALSE),
            selected = TRUE,
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          confInterval = list(
            inputId = "confInterval",
            label = "Confidence interval",
            choices = c(TRUE, FALSE),
            selected = TRUE,
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          rotateXLabels = list(
            inputId = "rotateXLabels",
            label = "Rotate x-axis labels",
            choices = c(TRUE, FALSE),
            selected = FALSE
          ),
          line = list(
            inputId = "line",
            label = "Line",
            value = FALSE
          )
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace
      )
    },

    .initTableInputs = function() {
      headerColumnOptions <- c("cdm_name", "estimate_name")

      groupColumnOptions <- c("outcome_cohort_name", "cdm_name")

      settingColumnOptions <- c(
        "denominator_time_at_risk", "denominator_age_group", "denominator_sex"
      )

      hideColumnOptions <- c(
        "denominator_time_at_risk", "denominator_cohort_name", "denominator_age_group",
        "denominator_sex", "analysis_interval"
      )

      private$.pickers[["table"]] <- InputPanel$new(
        funs = list(
          headerColumn = shinyWidgets::pickerInput,
          groupColumn = shinyWidgets::pickerInput,
          settingsColumn = shinyWidgets::pickerInput,
          hideColumn = shinyWidgets::pickerInput
        ),
        args = list(
          headerColumn = list(
            inputId = "headerColumn",
            choices = headerColumnOptions,
            label = "Header",
            selected = headerColumnOptions,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          groupColumn = list(
            inputId = "groupColumn",
            choices = groupColumnOptions,
            label = "Group columns",
            selected = groupColumnOptions[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          settingsColumn = list(
            inputId = "settingsColumn",
            choices = settingColumnOptions,
            label = "Settings columns",
            selected = settingColumnOptions,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          hideColumn = list(
            inputId = "hideColumn",
            choices = hideColumnOptions,
            label = "Hide columns",
            selected = hideColumnOptions,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace
      )
    },

    ## Helpers ----
    .getColValues = function(colName) {
      if (is.null(private$.settings[[colName]])) {
        NULL
      } else {
        private$.settings |>
          dplyr::distinct(.data[[colName]]) |>
          dplyr::pull(.data[[colName]])
      }
    },

    .setFilterValues = function() {
      private$.cdmNames <- getCDMNames(private$.result)
      private$.strata <- c("overall", getStrata(private$.result))
      private$.setCohortNames()

      private$.ageGroups <- private$.getColValues("denominator_age_group")
      private$.sex <- private$.getColValues("denominator_sex")
      private$.priorObs <- private$.getColValues("denominator_days_prior_observation")
      private$.startDate <- private$.getColValues("denominator_start_date")
      private$.endDate <- private$.getColValues("denominator_end_date")
      private$.timeAtRisk <- private$.getColValues("denominator_time_at_risk")

      private$.setDateRange()
      private$.setTimeIntervals()

      private$.databaseIntervals <- private$.getColValues("analysis_complete_database_intervals")

      # Incidence
      private$.outcomeWashout <- private$.getColValues("analysis_outcome_washout")
      private$.repeatedEvents <- private$.getColValues("analysis_repeated_events")

      # Point Prev
      # -

      # Period Prev
      private$.fullContribution <- private$.getColValues("analysis_full_contribution")
      private$.level <- private$.getColValues("analysis_level")

      private$.hasInterval <- any(grepl("analysis_interval", private$.result$additional_name))
    },

    .setDateRange = function() {
      private$.dateRangeMin <- private$.result |>
        omopgenerics::filterAdditional(
          .data[[private$.startDateCol]] == min(.data[[private$.startDateCol]])
        ) |>
        omopgenerics::tidy() |>
        dplyr::distinct(.data[[private$.startDateCol]]) |>
        dplyr::pull() |>
        as.Date()

      private$.dateRangeMax <- private$.result |>
        omopgenerics::filterAdditional(.data[[private$.endDateCol]] != "overall") |>
        omopgenerics::filterAdditional(.data[[private$.endDateCol]] == max(.data[[private$.endDateCol]])) |>
        omopgenerics::tidy() |>
        dplyr::distinct(.data[[private$.endDateCol]]) |>
        dplyr::pull() |>
        as.Date()
    },

    .setTimeIntervals = function() {
      # "overall", "years", "quarters", "months", "weeks"
      private$.timeIntervals <- if (private$.hasInterval) {
        additionalLevels <- private$.result |>
          dplyr::filter(grepl(pattern = "analysis_interval", x = .data$additional_name)) |>
          dplyr::distinct(.data$additional_level) |>
          dplyr::pull(.data$additional_level)

        additionalItems <- stringr::str_split(string = additionalLevels, pattern = " &&& ") |>
          unlist() |>
          unique()

        additionalItems[grepl(pattern = "^[a-z]", x = tolower(additionalItems))]
      } else {
        "overall"
      }
    },

    .setCohortNames = function() {
      groupLevels <- private$.result |>
        dplyr::filter(grepl(pattern = "outcome_cohort_name", x = .data$group_name)) |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)

      cohortNames <- stringr::str_split(string = groupLevels, pattern = " &&& ") |>
        unlist() |>
        unique()

      private$.denominatorCohorts <- cohortNames[stringr::str_detect(string = cohortNames, pattern = "^denom")]
      private$.outcomeCohorts <- cohortNames[!cohortNames %in% private$.denominatorCohorts]
    },

    .setIncidenceCols = function() {
      private$.resultType <- "incidence"
      private$.startDateCol <- "incidence_start_date"
      private$.endDateCol <- "incidence_end_date"
      private$.estimate <- "incidence_100000_pys"
      private$.ciUpper <- "incidence_100000_pys_95CI_lower"
      private$.ciLower <- "incidence_100000_pys_95CI_upper"
    },

    .setPrevalenceCols = function() {
      analysisType <- unique(private$.settings$analysis_type)

      private$.startDateCol <- "prevalence_start_date"
      private$.endDateCol <- "prevalence_end_date"
      private$.estimate <- "prevalence"
      private$.ciUpper <- "prevalence_95CI_lower"
      private$.ciLower <- "prevalence_95CI_upper"

      if (analysisType == "point prevalence") {
        private$.resultType <- "point_prev"
      } else if (analysisType == "period prevalence") {
        private$.resultType <- "period_prev"
      }
    },

    .plotIncidencePrevalence = function(line, confInterval, ...) {
      gg <- if (private$.resultType == "incidence") {
        IncidencePrevalence::plotIncidence(...)
      } else {
        IncidencePrevalence::plotPrevalence(...)
      }

      if (!confInterval) {
        gg$layers <- gg$layers[2]
      }

      if (line) {
        gg <- gg +
          ggplot2::geom_line()
      }

      gg <- gg +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))

      return(gg)
    },

    .tableIncidencePrevalence = function(...) {
      if (private$.resultType == "incidence") {
        IncidencePrevalence::tableIncidence(...)
      } else {
        IncidencePrevalence::tablePrevalence(...)
      }
    },

    .assertResult = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!any(resSettings$result_type %in% c("incidence", "prevalence", "incidence_attrition", "prevalence_attrition"))) {
        stop("Cannot assert `Incidence` or `Prevalence` result")
      }
    }
  )
)
