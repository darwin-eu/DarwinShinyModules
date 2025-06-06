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
#'     incMod <- Incidence$new(data = inc)
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
    #' @field data (`summarisedResult`) SummarisedResult object from Incidence.
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        # Checks on data
        checkmate::assertClass(data, "summarised_result")
        private$.data <- data
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
    #' @param data (`summarised_result`) Result object from the `IncidencePrevalence` package.
    #'
    #' @returns `self`
    initialize = function(data) {
      super$initialize()
      private$assertInstall("IncidencePrevalence", "1.2.0")
      private$assertInstall("visOmopResults", "1.0.2")
      private$assertIncidenceData(data)
      private$.data <- private$transformData(data)
      private$initPickers()
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .pickers = NULL,
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "incidence"),
          shiny::h3("Incidence estimates"),
          shiny::p("Incidence estimates are shown below, please select configuration to filter them:"),
          shiny::p("Database and study outcome"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          p("Denominator population settings"),
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["denomSex"]]$UI(),
          private$.pickers[["denomPriorObs"]]$UI(),
          private$.pickers[["denomStartDate"]]$UI(),
          private$.pickers[["denomEndDate"]]$UI(),
          private$.pickers[["denomTimeAtRisk"]]$UI(),
          p("Analysis settings"),
          private$.pickers[["washout"]]$UI(),
          private$.pickers[["repeatedEvents"]]$UI(),
          private$.pickers[["completePeriod"]]$UI(),
          private$.pickers[["minCounts"]]$UI(),
          p("Dates"),
          private$.pickers[["interval"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Table of estimates",
              shiny::downloadButton(shiny::NS(private$.namespace, "download_table"), "Download current estimates"),
              DT::DTOutput(shiny::NS(private$.namespace, "table")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot of estimates",
              p("Plotting options"),
              private$.pickers[["xAxis"]]$UI(),
              private$.pickers[["facet"]]$UI(),
              private$.pickers[["color"]]$UI(),
              private$.pickers[["ribbon"]]$UI(),
              plotly::plotlyOutput(
                shiny::NS(private$.namespace, "plot"),
                height = "800px"
              ) %>%
                shinycssloaders::withSpinner(),
              shiny::h4("Download figure"),
              shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_height"), "", 10, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_width"), "", 20, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block; margin-right:",
                shiny::textInput(shiny::NS(private$.namespace, "download_dpi"), "", 300, width = "50px")
              ),
              shiny::downloadButton(shiny::NS(private$.namespace, "download_plot"), "Download plot")
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }

      # Incidence
      getIncidenceEstimates <- reactive({
        result <- private$.data %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs) %>%
          dplyr::filter(denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date) %>%
          dplyr::filter(denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date) %>%
          dplyr::filter(denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          dplyr::filter(analysis_outcome_washout %in% private$.pickers[["washout"]]$inputValues$washout) %>%
          dplyr::filter(analysis_repeated_events %in% private$.pickers[["repeatedEvents"]]$inputValues$repeated_events) %>%
          dplyr::filter(analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period) %>%
          dplyr::filter(analysis_min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count) %>%
          dplyr::filter(analysis_interval %in% private$.pickers[["interval"]]$inputValues$interval) %>%
          dplyr::filter(incidence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          dplyr::mutate(
            person_years = round(suppressWarnings(as.numeric(person_years))),
            person_days = round(suppressWarnings(as.numeric(person_days))),
            n_events = round(suppressWarnings(as.numeric(n_events))),
            incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
            incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
            incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
          )
        return(result)
      })

      ### download table ----
      output$download_table <- downloadHandler(
        filename = function() {
          "incidenceEstimatesTable.csv"
        },
        content = function(file) {
          utils::write.csv(getIncidenceEstimates(), file)
        }
      )

      ### table estimates ----
      output$table <- DT::renderDT({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))

        table <- table %>%
          mutate(incidence_100000_pys = paste0(
            incidence_100000_pys, " (", incidence_100000_pys_95CI_lower, " to ",
            incidence_100000_pys_95CI_upper, " )"
          )) %>%
          select(database, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, denominator_time_at_risk, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)

        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })

      ### make plot ----
      plotIncidenceEstimates <- reactive({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))

        IncidencePrevalence::plotIncidence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "incidence_100000_pys",
          line = FALSE,
          point = TRUE,
          ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
          ymin = "incidence_100000_pys_95CI_lower",
          ymax = "incidence_100000_pys_95CI_upper",
          facet = private$.pickers[["facet"]]$inputValues$facet_by,
          colour = private$.pickers[["color"]]$inputValues$color_by
        )
      })

      ### download plot ----
      output$download_plot <- downloadHandler(
        filename = function() {
          "incidenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            plotIncidenceEstimates(),
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- renderPlotly({
        plotIncidenceEstimates()
      })
    },
    assertIncidenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("incidence", "incidence_attrition"))) {
        stop("Cannot assert `Incidence` result")
      }
    },
    transformData = function(data) {
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asIncidenceResult(data) %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_events = outcome_count,
          n_persons = denominator_count
        )
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = unique(private$.data$database), selected = unique(private$.data$database), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace

      # outcome
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = unique(private$.data$outcome_cohort_name), selected = unique(private$.data$outcome_cohort_name), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace

      # denominator age group
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Age group", choices = unique(private$.data$denominator_age_group), selected = unique(private$.data$denominator_age_group), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomAgeGroup"]]$parentNamespace <- self$namespace

      # denominator sex
      private$.pickers[["denomSex"]] <- InputPanel$new(
        funs = list(denom_sex = shinyWidgets::pickerInput),
        args = list(denom_sex = list(
          inputId = "denom_sex", choices = unique(private$.data$denominator_sex), label = "Sex", selected = unique(private$.data$denominator_sex), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomSex"]]$parentNamespace <- self$namespace

      # prior observation
      private$.pickers[["denomPriorObs"]] <- InputPanel$new(
        funs = list(prior_obs = shinyWidgets::pickerInput),
        args = list(prior_obs = list(
          inputId = "prior_obs", choices = unique(private$.data$denominator_days_prior_observation), label = "Prior observation", selected = unique(private$.data$denominator_days_prior_observation), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomPriorObs"]]$parentNamespace <- self$namespace

      # denominator start date
      private$.pickers[["denomStartDate"]] <- InputPanel$new(
        funs = list(start_date = shinyWidgets::pickerInput),
        args = list(start_date = list(
          inputId = "start_date", choices = unique(private$.data$denominator_start_date), label = "Start date", selected = unique(private$.data$denominator_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomStartDate"]]$parentNamespace <- self$namespace

      # denominator end date
      private$.pickers[["denomEndDate"]] <- InputPanel$new(
        funs = list(end_date = shinyWidgets::pickerInput),
        args = list(end_date = list(
          inputId = "end_date", choices = unique(private$.data$denominator_end_date), label = "End date", selected = unique(private$.data$denominator_end_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomEndDate"]]$parentNamespace <- self$namespace

      # denominator time at risk
      private$.pickers[["denomTimeAtRisk"]] <- InputPanel$new(
        funs = list(time_at_risk = shinyWidgets::pickerInput),
        args = list(time_at_risk = list(
          inputId = "time_at_risk", choices = unique(private$.data$denominator_time_at_risk), label = "Time at risk", selected = unique(private$.data$denominator_time_at_risk), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomTimeAtRisk"]]$parentNamespace <- self$namespace

      # washout
      private$.pickers[["washout"]] <- InputPanel$new(
        funs = list(washout = shinyWidgets::pickerInput),
        args = list(washout = list(
          inputId = "washout", choices = unique(private$.data$analysis_outcome_washout), label = "Outcome washout", selected = unique(private$.data$analysis_outcome_washout), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["washout"]]$parentNamespace <- self$namespace

      # repeated events
      private$.pickers[["repeatedEvents"]] <- InputPanel$new(
        funs = list(repeated_events = shinyWidgets::pickerInput),
        args = list(repeated_events = list(
          inputId = "repeated_events", choices = unique(private$.data$analysis_repeated_events), label = "Repeated events", selected = unique(private$.data$analysis_repeated_events), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["repeatedEvents"]]$parentNamespace <- self$namespace

      # complete period
      private$.pickers[["completePeriod"]] <- InputPanel$new(
        funs = list(complete_period = shinyWidgets::pickerInput),
        args = list(complete_period = list(
          inputId = "complete_period", choices = unique(private$.data$analysis_complete_database_intervals), label = "Complete period", selected = unique(private$.data$analysis_complete_database_intervals), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["completePeriod"]]$parentNamespace <- self$namespace

      # min counts
      private$.pickers[["minCounts"]] <- InputPanel$new(
        funs = list(min_cell_count = shinyWidgets::pickerInput),
        args = list(min_cell_count = list(
          inputId = "min_cell_count", choices = unique(private$.data$analysis_min_cell_count), label = "Minimum counts", selected = unique(private$.data$analysis_min_cell_count), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["minCounts"]]$parentNamespace <- self$namespace

      # interval
      private$.pickers[["interval"]] <- InputPanel$new(
        funs = list(interval = shinyWidgets::pickerInput),
        args = list(interval = list(
          inputId = "interval", choices = unique(private$.data$analysis_interval), label = "Interval", selected = unique(private$.data$analysis_interval)[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["interval"]]$parentNamespace <- self$namespace

      # start date
      private$.pickers[["startDate"]] <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(
          inputId = "year", choices = unique(private$.data$incidence_start_date), label = "Year", selected = unique(private$.data$incidence_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace

      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"
      )
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Incidence_start_date", selected = "incidence_start_date", multiple = F,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["xAxis"]]$parentNamespace <- self$namespace

      # facet by
      private$.pickers[["facet"]] <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name", "database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["facet"]]$parentNamespace <- self$namespace

      # color by
      private$.pickers[["color"]] <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c(), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["color"]]$parentNamespace <- self$namespace

      # ribbon
      private$.pickers[["ribbon"]] <- InputPanel$new(
        funs = list(ribbon = shinyWidgets::pickerInput),
        args = list(ribbon = list(
          inputId = "ribbon", choices = c(TRUE, FALSE), label = "Ribbon", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["ribbon"]]$parentNamespace <- self$namespace
    }
  )
)
