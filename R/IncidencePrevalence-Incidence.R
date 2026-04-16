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

#' @title Incidence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Incidence module that shows incidence results from the IncidencePrevalence package.
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
#'     incMod <- Incidence$new(result = inc,
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
Incidence <- R6::R6Class(
  classname = "Incidence",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarisedResult`) SummarisedResult object from Incidence.
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
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, defaults = list(), ...) {
      super$initialize(...)
      private$.assertIncidenceData(result)
      private$.result <- result
      private$.tidyData <- private$.transformData(result)
      private$.defaults <- defaults
      private$.initPickers()

      private$.table <- Flextable$new(
        fun = IncidencePrevalence::tableIncidence,
        args = list(
          type = "flextable",
          style = "darwin"
        ),
        height = "80vh",
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = IncidencePrevalence::plotIncidence,
        args = list(),
        height = "80vh",
        parentNamespace = self$namespace
      )

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .result = NULL,
    .tidyData = NULL,

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

    ## UI ----
    .UI = function() {
      shiny::tagList(
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
      shiny::tagList(
        shiny::h3("Incidence estimates"),
        shiny::p("Incidence estimates are shown below, please select configuration to filter them:"),
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
        shiny::column(
          width = 2,
          private$.pickers[["plot"]]$UI()
        ),
        shiny::column(
          width = 10,
          private$.plot$UI()
        )
      )
    },

    .uiTable = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          private$.pickers[["table"]]$UI()
        ),
        shiny::column(
          width = 10,
          private$.table$UI()
        )
      )
    },

    ## Server ----
    .serverTable = function(input, output, session, data) {
      inputValues <- private$.pickers[["table"]]$inputValues

      shiny::observe({
        shiny::req(data())

        private$.table$args$result <- data()
        private$.table$args$header <- inputValues$headerColumn
        private$.table$args$groupColumn <- inputValues$groupColumn
        private$.table$args$settingsColumn <- inputValues$settingsColumn
        private$.table$args$hide <- inputValues$hideColumn

        private$.table$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session, data) {
      shiny::observe({
        shiny::req(data())

        private$.plot$args$result <- data()
        private$.plot$args$x <- private$.pickers[["plot"]]$inputValues$xAxis
        private$.plot$args$y <- "incidence_100000_pys"
        private$.plot$args$line <- FALSE
        private$.plot$args$point <- TRUE
        private$.plot$args$ribbon <- as.logical(private$.pickers[["plot"]]$inputValues$ribbon)
        private$.plot$args$ymin <- "incidence_100000_pys_95CI_lower"
        private$.plot$args$ymax <- "incidence_100000_pys_95CI_upper"
        private$.plot$args$facet <- private$.pickers[["plot"]]$inputValues$facet_by
        private$.plot$args$colour <- private$.pickers[["plot"]]$inputValues$color_by

        private$.plot$server(input, output, session)
      })
    },

    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      # Incidence
      getIncidenceEstimates <- reactive({
        private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["database"]]$inputValues$cdm) |>
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["database"]]$inputValues$outcome) |>
          dplyr::filter(strata %in% private$.pickers[["database"]]$inputValues$strata) |>

          dplyr::filter(denominator_age_group %in% private$.pickers[["denominator"]]$inputValues$age_group) |>
          dplyr::filter(denominator_sex %in% private$.pickers[["denominator"]]$inputValues$denom_sex) |>
          dplyr::filter(denominator_days_prior_observation %in% private$.pickers[["denominator"]]$inputValues$prior_obs) |>
          dplyr::filter(denominator_start_date %in% private$.pickers[["denominator"]]$inputValues$start_date) |>
          dplyr::filter(denominator_end_date %in% private$.pickers[["denominator"]]$inputValues$end_date) |>
          dplyr::filter(denominator_time_at_risk %in% private$.pickers[["denominator"]]$inputValues$time_at_risk) |>

          dplyr::filter(analysis_outcome_washout %in% private$.pickers[["analysis"]]$inputValues$washout) |>
          dplyr::filter(analysis_repeated_events %in% private$.pickers[["analysis"]]$inputValues$repeated_events) |>
          dplyr::filter(analysis_complete_database_intervals %in% private$.pickers[["analysis"]]$inputValues$complete_period) |>
          dplyr::filter(analysis_min_cell_count %in% private$.pickers[["analysis"]]$inputValues$min_cell_count) |>

          dplyr::filter(analysis_interval %in% private$.pickers[["date"]]$inputValues$interval) |>
          dplyr::filter(incidence_start_date %in% private$.pickers[["date"]]$inputValues$year) |>
          dplyr::mutate(
            person_years = round(suppressWarnings(as.numeric(person_years))),
            person_days = round(suppressWarnings(as.numeric(person_days))),
            n_events = round(suppressWarnings(as.numeric(n_events))),
            incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
            incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
            incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
          )
      })

      # Filtered data
      summarised_result_data <- reactive({
        private$.result %>%
          dplyr::filter(
          cdm_name %in% private$.pickers[["database"]]$inputValues$cdm) %>%
          omopgenerics::filterSettings(
            analysis_repeated_events %in% private$.pickers[["analysis"]]$inputValues$repeated_events,
            analysis_outcome_washout %in% private$.pickers[["analysis"]]$inputValues$washout,
            analysis_complete_database_intervals %in% private$.pickers[["analysis"]]$inputValues$complete_period,
            min_cell_count %in% private$.pickers[["analysis"]]$inputValues$min_cell_count,
            denominator_start_date %in% private$.pickers[["denominator"]]$inputValues$start_date,
            denominator_end_date %in% private$.pickers[["denominator"]]$inputValues$end_date,
            denominator_days_prior_observation %in% private$.pickers[["denominator"]]$inputValues$prior_obs,
            denominator_sex %in% private$.pickers[["denominator"]]$inputValues$denom_sex,
            denominator_age_group %in% private$.pickers[["denominator"]]$inputValues$age_group,
            denominator_time_at_risk %in% private$.pickers[["denominator"]]$inputValues$time_at_risk
          ) |>
          omopgenerics::filterAdditional(
            analysis_interval == private$.pickers[["date"]]$inputValues$interval,
            incidence_start_date %in% private$.pickers[["date"]]$inputValues$year
          ) |>
          omopgenerics::filterGroup(outcome_cohort_name %in% private$.pickers[["database"]]$inputValues$outcome)
      })

      # TABLE
      private$.serverTable(input, output, session, data = summarised_result_data)

      private$.serverPlot(input, output, session, data = getIncidenceEstimates)

      ### make plot ----
      # plotIncidenceEstimates <- reactive({
      #   table <- getIncidenceEstimates()
      #   shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
      #   class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
      #
      #   plot <- IncidencePrevalence::plotIncidence(
      #     result = table,
      #     x = private$.pickers[["xAxis"]]$inputValues$xAxis,
      #     y = "incidence_100000_pys",
      #     line = FALSE,
      #     point = TRUE,
      #     ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
      #     ymin = "incidence_100000_pys_95CI_lower",
      #     ymax = "incidence_100000_pys_95CI_upper",
      #     facet = private$.pickers[["facet"]]$inputValues$facet_by,
      #     colour = private$.pickers[["color"]]$inputValues$color_by
      #   )
      #   # remove confidence interval
      #   if (!as.logical(private$.pickers[["confInterval"]]$inputValues$confInterval)) {
      #     plot$layers <- plot$layers[2]
      #     if (as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon)) {
      #       plot <- plot + ggplot2::geom_line()
      #     }
      #   }
      #   if (as.logical(private$.pickers[["rotateXLabels"]]$inputValues$rotateXLabels)) {
      #     plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1))
      #   }
      #   plot
      # })

      ### download plot ----
      # output$download_plot <- downloadHandler(
      #   filename = function() {
      #     "incidenceEstimatesPlot.png"
      #   },
      #   content = function(file) {
      #     ggplot2::ggsave(
      #       file,
      #       plotIncidenceEstimates(),
      #       width = as.numeric(input$download_width),
      #       height = as.numeric(input$download_height),
      #       dpi = as.numeric(input$download_dpi),
      #       units = "cm"
      #     )
      #   }
      # )
      # ### plot ----
      # output$plot <- renderPlotly({
      #   plotIncidenceEstimates()
      # })
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
      allDatabases <- unique(private$.tidyData$database)
      selectedDatabases <- allDatabases[1]
      databaseStr <- "database"
      if (databaseStr %in% names(private$.defaults) && all(private$.defaults[[databaseStr]] %in% allDatabases)) {
        selectedDatabases <- private$.defaults[[databaseStr]]
      }

      allOutcomes <- unique(private$.tidyData$outcome_cohort_name)
      selectedOutcomes <- allOutcomes[1]
      outcomeStr <- "outcome"
      if (outcomeStr %in% names(private$.defaults) && all(private$.defaults[[outcomeStr]] %in% allOutcomes)) {
        selectedOutcomes <- private$.defaults[[outcomeStr]]
      }

      allStrata <- unique(private$.strata)
      strataStr <- "strata"
      selectedStrata <- allStrata[1]
      if (strataStr %in% names(private$.defaults) && all(private$.defaults[[strataStr]] %in% allStrata)) {
        selectedStrata <- private$.defaults[[strataStr]]
      }

      private$.pickers[["database"]] <- InputPanel$new(
        funs = list(
          cdm = shinyWidgets::pickerInput,
          outcome = shinyWidgets::pickerInput,
          strata = shinyWidgets::pickerInput
        ),
        args = list(
          cdm = list(
            inputId = "cdm",
            label = "Database",
            choices = allDatabases,
            selected = selectedDatabases,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          outcome = list(
            inputId = "outcome",
            label = "Outcome",
            choices = allOutcomes,
            selected = selectedOutcomes,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          strata = list(
            inputId = "strata",
            label = "Strata",
            choices = allStrata,
            selected = selectedStrata,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initDenominatorInputs = function() {
      # denominator age group
      allAgeGroups <- unique(private$.tidyData$denominator_age_group)
      selectedAgeGroups <- allAgeGroups[1]
      ageGroupStr <- "ageGroup"
      if (ageGroupStr %in% names(private$.defaults) && all(private$.defaults[[ageGroupStr]] %in% allAgeGroups)) {
        selectedAgeGroups <- private$.defaults[[ageGroupStr]]
      }

      allSex <- unique(private$.tidyData$denominator_sex)
      selectedSex <- allSex[1]
      sexStr <- "sex"
      if (sexStr %in% names(private$.defaults) && all(private$.defaults[[sexStr]] %in% allSex)) {
        selectedSex <- private$.defaults[[sexStr]]
      }

      allPO <- unique(private$.tidyData$denominator_days_prior_observation)
      selectedPO <- allPO[1]
      priorObservationStr <- "prior_observation"
      if (priorObservationStr %in% names(private$.defaults) && all(private$.defaults[[priorObservationStr]] %in% allPO)) {
        selectedPO <- private$.defaults[[priorObservationStr]]
      }

      allDenomStartDates <- unique(private$.tidyData$denominator_start_date)
      selectedDenomStartDate <- allDenomStartDates[1]
      startDateStr <- "start_date"
      if (startDateStr %in% names(private$.defaults) && all(private$.defaults[[startDateStr]] %in% allDenomStartDates)) {
        selectedDenomStartDate <- private$.defaults[[startDateStr]]
      }

      allDenomEndDates <- unique(private$.tidyData$denominator_end_date)
      selectedDenomEndDate <- allDenomEndDates[1]
      endDateStr <- "end_date"
      if (endDateStr %in% names(private$.defaults) && all(private$.defaults[[endDateStr]] %in% allDenomEndDates)) {
        selectedDenomEndDate <- private$.defaults[[endDateStr]]
      }

      allTimeAtRisk <- unique(private$.tidyData$denominator_time_at_risk)
      selectedTimeAtRisk <- allTimeAtRisk[1]
      timeAtRiskStr <- "time_at_risk"
      if (timeAtRiskStr %in% names(private$.defaults) && all(private$.defaults[[timeAtRiskStr]] %in% allTimeAtRisk)) {
        selectedTimeAtRisk <- private$.defaults[[timeAtRiskStr]]
      }

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
            choices = allAgeGroups,
            selected = selectedAgeGroups,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          denom_sex = list(
            inputId = "denom_sex",
            choices = allSex,
            label = "Sex",
            selected = selectedSex,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          prior_obs = list(
            inputId = "prior_obs",
            choices = allPO,
            label = "Prior observation",
            selected = selectedPO,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          start_date = list(
            inputId = "start_date",
            choices = allDenomStartDates,
            label = "Start date",
            selected = selectedDenomStartDate,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          end_date = list(
            inputId = "end_date",
            choices = allDenomEndDates,
            label = "End date",
            selected = selectedDenomEndDate,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          time_at_risk = list(
            inputId = "time_at_risk",
            choices = allTimeAtRisk,
            label = "Time at risk",
            selected = selectedTimeAtRisk,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initAnalysisInputs = function() {
      # washout
      allWashout <- unique(private$.tidyData$analysis_outcome_washout)
      selectedWashout <- allWashout[1]
      washoutStr <- "washout"
      if (washoutStr %in% names(private$.defaults) && all(private$.defaults[[washoutStr]] %in% allWashout)) {
        selectedWashout <- private$.defaults[[washoutStr]]
      }

      allRepeatedEvents <- unique(private$.tidyData$analysis_repeated_events)
      selectedRepeatedEvents <- allRepeatedEvents[1]
      repeatedEventsStr <- "repeated_events"
      if (repeatedEventsStr %in% names(private$.defaults) && all(private$.defaults[[repeatedEventsStr]] %in% allRepeatedEvents)) {
        selectedRepeatedEvents <- private$.defaults[[repeatedEventsStr]]
      }

      allCompletePeriod <- unique(private$.tidyData$analysis_complete_database_intervals)
      selectedCompletePeriod <- allCompletePeriod[1]
      completePeriodStr <- "complete_period"
      if (completePeriodStr %in% names(private$.defaults) && all(private$.defaults[[completePeriodStr]] %in% allCompletePeriod)) {
        selectedCompletePeriod <- private$.defaults[[completePeriodStr]]
      }

      allMinCounts <- unique(private$.tidyData$analysis_min_cell_count)
      selectedMinCounts <- allMinCounts[1]
      minCellCountStr <- "min_cell_count"
      if (minCellCountStr %in% names(private$.defaults) && private$.defaults[[minCellCountStr]] %in% allMinCounts) {
        selectedMinCounts <- private$.defaults[[minCellCountStr]]
      }

      private$.pickers[["analysis"]] <- InputPanel$new(
        funs = list(
          washout = shinyWidgets::pickerInput,
          repeated_events = shinyWidgets::pickerInput,
          complete_period = shinyWidgets::pickerInput,
          min_cell_count = shinyWidgets::pickerInput
        ),
        args = list(
          washout = list(
            inputId = "washout",
            choices = allWashout,
            label = "Outcome washout",
            selected = selectedWashout,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          repeated_events = list(
            inputId = "repeated_events",
            choices = allRepeatedEvents,
            label = "Repeated events",
            selected = selectedRepeatedEvents,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          complete_period = list(
            inputId = "complete_period",
            choices = allCompletePeriod,
            label = "Complete period",
            selected = selectedCompletePeriod,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          min_cell_count = list(
            inputId = "min_cell_count",
            choices = allMinCounts,
            label = "Minimum counts",
            selected = selectedMinCounts,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initDateInputs = function() {
      allIntervals <- unique(private$.tidyData$analysis_interval)
      allIntervals <- na.omit(allIntervals[order(match(allIntervals, c("overall", "years", "quarters", "months", "weeks")))])
      selectedInterval <- allIntervals[1]
      intervalStr <- "interval"
      if (intervalStr %in% names(private$.defaults) && all(private$.defaults[[intervalStr]] %in% allIntervals)) {
        selectedInterval <- private$.defaults[[intervalStr]]
      }

      allStartDates <- unique(private$.tidyData$incidence_start_date)
      selectedStartDate <- allStartDates[1]
      intervalStartDateStr <- "interval_start_date"
      if (intervalStartDateStr %in% names(private$.defaults) && all(private$.defaults[[intervalStartDateStr]] %in% allStartDates)) {
        selectedStartDate <- private$.defaults[[intervalStartDateStr]]
      }

      private$.pickers[["date"]] <- InputPanel$new(
        funs = list(
          interval = shinyWidgets::pickerInput,
          year = shinyWidgets::pickerInput
        ),
        args = list(
          interval = list(
            inputId = "interval",
            choices = allIntervals,
            label = "Interval",
            selected = selectedInterval,
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          year = list(
            inputId = "year",
            choices = allStartDates,
            label = "Year",
            selected = selectedStartDate,
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initPlotInputs = function() {
      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "strata", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"
      )

      private$.pickers[["plot"]] <- InputPanel$new(
        funs = list(
          xAxis = shinyWidgets::pickerInput,
          facet_by = shinyWidgets::pickerInput,
          color_by = shinyWidgets::pickerInput,
          ribbon = shinyWidgets::pickerInput,
          confInterval = shinyWidgets::pickerInput,
          rotateXLabels = shinyWidgets::pickerInput
        ),
        args = list(
          xAxis = list(
            inputId = "xAxis",
            choices = plotDataChoices,
            label = "Incidence_start_date",
            selected = "incidence_start_date",
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          facet_by = list(
            inputId = "facet_by",
            choices = plotDataChoices,
            label = "Facet by",
            selected = c("outcome_cohort_name", "database"),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          color_by = list(
            inputId = "color_by",
            choices = plotDataChoices,
            label = "Colour by",
            selected = c(),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          ribbon = list(
            inputId = "ribbon",
            choices = c(TRUE, FALSE),
            label = "Ribbon",
            selected = TRUE,
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          confInterval = list(
            inputId = "confInterval",
            choices = c(TRUE, FALSE),
            label = "Confidence interval",
            selected = TRUE,
            multiple = FALSE,
            options = private$.pickerOptions
          ),
          rotateXLabels = list(
            inputId = "rotateXLabels",
            choices = c(TRUE, FALSE),
            label = "Rotate x-axis labels",
            selected = FALSE
          )
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace
      )
    },

    .initTableInputs = function() {
      headerColumnOptions <- c("cdm_name", "estimate_name")
      groupColumnOptions <- c("outcome_cohort_name", "cdm_name")
      settingColumnOptions <- c("denominator_time_at_risk", "denominator_age_group", "denominator_sex")
      hideColumnOptions <- c("denominator_time_at_risk", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "analysis_interval")

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
    .assertIncidenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("incidence", "incidence_attrition"))) {
        stop("Cannot assert `Incidence` result")
      }
    },

    .transformData = function(data) {
      # set strata
      strataColumn <- unique(omopgenerics::settings(data) %>% dplyr::filter(strata != "reason") %>% dplyr::pull(strata))
      private$.strata <- unique(data %>% dplyr::filter(strata_name != "reason") %>% dplyr::pull(strata_level))

      # transform to readable format
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asIncidenceResult(data) %>%
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_events = outcome_count,
          n_persons = denominator_count
        )
      # add strata column
      if (strataColumn == "") {
        data %>%
          dplyr::mutate(strata = "overall")
      } else {
        data %>%
          dplyr::rename(strata = strataColumn)
      }
    }
  )
)
