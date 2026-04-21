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
      private$.settings <- omopgenerics::settings(result)
      private$.tidyData <- private$.transformData(result)
      private$.defaults <- defaults
      private$.setFilterValues()
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
        fun = private$.plotIncidence,
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
    .settings = NULL,
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

    .cdmNames = NULL,
    .denominatorCohorts = NULL,
    .outcomeCohorts = NULL,

    .ageGroups = NULL,
    .sex = NULL,
    .priorObs = NULL,
    .startDate = NULL,
    .endDate = NULL,
    .timeAtRisk = NULL,

    .outcomeWashout = NULL,
    .repeatedEvents = NULL,
    .databaseIntervals = NULL,
    .minCellCount = NULL,

    .timeIntervals = NULL,

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
      shiny::observeEvent(private$.pickers[["date"]]$inputValues$interval, {
        interval <- private$.pickers[["date"]]$inputValues$interval

        additionalLevels <- result |>
          dplyr::filter(grepl(pattern = "analysis_interval", x = .data$additional_name)) |>
          dplyr::filter(grepl(pattern = interval, x = .data$additional_level)) |>
          dplyr::distinct(.data$additional_level) |>
          dplyr::pull(.data$additional_level)

        additionalLevelItems <- stringr::str_split(string = additionalLevels, pattern = " &&& ") |>
          unlist() |>
          unique()

        dates <- additionalLevelItems[grepl(pattern = "^\\d", x = additionalLevelItems)] |>
          as.Date()

        choices <- if (interval == "years") {
          dates |>
            format("%Y") |>
            unique()
        } else if (interval == "months") {
          if (FALSE) {
            month.name
          } else {
            dates |>
              unique() |>
              as.character()
          }
        } else if (inteval == "quarters") {
          dates
        } else if (interval == "weeks") {
          dates
        }

        private$.pickers[["date"]]$update(
          fun = shinyWidgets::updatePickerInput,
          name = "intervalItems",
          session = session,
          choices = choices,
          selected = choices[1]
        )
      })
    },

    .serverGetSummariseResultData = function(input, output, session) {
      reactive({
        browser()
        df <- private$.result %>%
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
            analysis_interval == private$.pickers[["date"]]$inputValues$interval
          ) |>
          omopgenerics::filterGroup(outcome_cohort_name %in% private$.pickers[["database"]]$inputValues$outcome)

        # "overall", "years", "quarters", "months", "weeks"
        if (private$.pickers[["date"]]$inputValues$interval == "years") {
          pat <- sprintf("[%s]", paste(private$.pickers[["date"]]$inputValues$intervalItems, collapse = "|"))
          df <- df |>
            omopgenerics::filterAdditional(
              grepl(x = .data$incidence_start_date, pattern = pat)
            )
        } else if (private$.pickers[["date"]]$inputValues$interval == "months") {
          df <- df |>
            omopgenerics::filterAdditional(
              .data$incidence_start_date %in% private$.pickers[["date"]]$inputValues$intervalItems
            )
        } else if (private$.pickers[["date"]]$inputValues$interval == "quarters") {
          df <- df
        } else if (private$.pickers[["date"]]$inputValues$interval == "weeks") {
          df <- df
        } else if (private$.pickers[["date"]]$inputValues$interval == "months seasonal") {
          df <- df
        } else if (private$.pickers[["date"]]$inputValues$interval == "quarters seasonal") {
          df <- df
        } else if (private$.pickers[["date"]]$inputValues$interval == "weeks seasonal") {
          df <- df
        } else {
          df <- df
        }
        return(df)
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
        private$.plot$args$y <- "incidence_100000_pys"
        private$.plot$args$line <- FALSE
        private$.plot$args$point <- TRUE
        private$.plot$args$ribbon <- as.logical(private$.pickers[["plot"]]$inputValues$ribbon)
        private$.plot$args$ymin <- "incidence_100000_pys_95CI_lower"
        private$.plot$args$ymax <- "incidence_100000_pys_95CI_upper"
        private$.plot$args$facet <- private$.pickers[["plot"]]$inputValues$facet_by
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
          ),
          min_cell_count = list(
            inputId = "min_cell_count",
            label = "Minimum counts",
            choices = private$.minCellCount,
            selected = private$.minCellCount[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        growDirection = "horizontal",
        parentNamespace = self$namespace
      )
    },

    .initDateInputs = function() {
      private$.pickers[["date"]] <- InputPanel$new(
        funs = list(
          interval = shinyWidgets::pickerInput,
          intervalItems = shinyWidgets::pickerInput
        ),
        args = list(
          interval = list(
            inputId = "interval",
            choices = private$.timeIntervals,
            label = "Interval",
            selected = private$.timeIntervals[1],
            options = private$.pickerOptions
          ),
          intervalItems = list(
            inputId = "intervalItems",
            label = "Intervals",
            choices = c(),
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
          rotateXLabels = shinyWidgets::pickerInput,
          line = shiny::checkboxInput
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
            selected = c("outcome_cohort_name", "cdm_name"),
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
    .getColValues = function(colName) {
      private$.settings |>
        dplyr::distinct(.data[[colName]]) |>
        dplyr::pull(.data[[colName]])
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

      private$.outcomeWashout <- private$.getColValues("analysis_outcome_washout")
      private$.repeatedEvents <- private$.getColValues("analysis_repeated_events")
      private$.databaseIntervals <- private$.getColValues("analysis_complete_database_intervals")
      private$.minCellCount <- private$.getColValues("min_cell_count")

      private$.setTimeIntervals()
    },

    .setTimeIntervals = function() {
      # "overall", "years", "quarters", "months", "weeks"
      additionalLevels <- private$.result |>
        dplyr::filter(grepl(pattern = "analysis_interval", x = .data$additional_name)) |>
        dplyr::distinct(.data$additional_level) |>
        dplyr::pull(.data$additional_level)

      additionalItems <- stringr::str_split(string = additionalLevels, pattern = " &&& ") |>
        unlist() |>
        unique()

      private$.timeIntervals <- additionalItems[grepl(pattern = "^[a-z]", x = tolower(additionalItems))]
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

    .plotIncidence = function(line, confInterval, ...) {
      gg <- IncidencePrevalence::plotIncidence(...)

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
