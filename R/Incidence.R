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
      private$assertInstall()
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
    .cdmPicker = NULL,
    .outcomePicker = NULL,
    .denomAgeGroupPicker = NULL,
    .denomSexPicker = NULL,
    .denomPriorObsPicker = NULL,
    .denomStartDatePicker = NULL,
    .denomEndDatePicker = NULL,
    .outcomeWashoutPicker = NULL,
    .repeatedEventsPicker = NULL,
    .completePeriodPicker = NULL,
    .minCountsPicker = NULL,
    .intervalPicker = NULL,
    .incStartDatePicker = NULL,
    .treatmentPicker = NULL,
    .xAxisPicker = NULL,
    .facetByPicker = NULL,
    .colorByPicker = NULL,
    .ribbonPicker = NULL,
    .confIntervalPicker = NULL,

    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "incidence"),
          shiny::h3("Incidence estimates"),
          shiny::p("Incidence estimates are shown below, please select configuration to filter them:"),
          shiny::p("Database and study outcome"),
          private$.cdmPicker$UI(),
          private$.outcomePicker$UI(),
          p("Denominator population settings"),
          private$.denomAgeGroupPicker$UI(),
          private$.denomSexPicker$UI(),
          private$.denomPriorObsPicker$UI(),
          private$.denomStartDatePicker$UI(),
          private$.denomEndDatePicker$UI(),
          p("Analysis settings"),
          private$.outcomeWashoutPicker$UI(),
          private$.repeatedEventsPicker$UI(),
          private$.completePeriodPicker$UI(),
          private$.minCountsPicker$UI(),
          p("Dates"),
          private$.intervalPicker$UI(),
          private$.incStartDatePicker$UI(),

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
              private$.xAxisPicker$UI(),
              private$.facetByPicker$UI(),
              private$.colorByPicker$UI(),
              private$.ribbonPicker$UI(),
              private$.confIntervalPicker$UI(),
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
          dplyr::filter(database %in% private$.cdmPicker$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.outcomePicker$inputValues$outcome) %>%
          dplyr::filter(denominator_age_group %in% private$.denomAgeGroupPicker$inputValues$age_group) %>%
          dplyr::filter(denominator_sex %in% private$.denomSexPicker$inputValues$denom_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% private$.denomPriorObsPicker$inputValues$prior_obs) %>%
          dplyr::filter(denominator_start_date %in% private$.denomStartDatePicker$inputValues$start_date) %>%
          dplyr::filter(denominator_end_date %in% private$.denomEndDatePicker$inputValues$end_date) %>%
          dplyr::filter(analysis_outcome_washout %in% private$.outcomeWashoutPicker$inputValues$washout) %>%
          dplyr::filter(analysis_repeated_events %in% private$.repeatedEventsPicker$inputValues$repeated_events) %>%
          dplyr::filter(analysis_complete_database_intervals %in% private$.completePeriodPicker$inputValues$complete_period) %>%
          dplyr::filter(analysis_min_cell_count %in% private$.minCountsPicker$inputValues$min_cell_count) %>%
          dplyr::filter(analysis_interval %in% private$.intervalPicker$inputValues$interval) %>%
          dplyr::filter(incidence_start_date %in% private$.incStartDatePicker$inputValues$year) %>%
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
          select(database, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)

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
        private$plotEstimates(
          result = table,
          x = private$.xAxisPicker$inputValues$xAxis,
          y = "incidence_100000_pys",
          ylim = c(0, NA),
          ytype = "count",
          ribbon = as.logical(private$.ribbonPicker$inputValues$ribbon),
          facet = private$.facetByPicker$inputValues$facet_by,
          colour = private$.colorByPicker$inputValues$color_by,
          colour_name = paste0(private$.colorByPicker$inputValues$color_by, collapse = "; "),
          options = list("hideConfidenceInterval" = !as.logical(private$.confIntervalPicker$inputValues$conf_interval))
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

    assertInstall = function() {
      if (!require("IncidencePrevalence", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        answer <- readline(prompt = "`IncidencePrevalence` is not installed, would you like to install from CRAN? (y/n)")
        if (substr(tolower(answer), start = 1, stop = 1) == "y") {
          utils::install.packages("IncidencePrevalence")
        } else if (substr(tolower(answer), start = 1, stop = 1) == "n") {
          stop("You can install `IncidencePrevalence` manually by running one of the following:\n  1. `install.packages('IncidencePrevalence')`\n  2. `remotes::install_github('darwin-eu/IncidencePrevalence')`")
        } else {
          stop("Your answer was not `y` or `n`")
        }
      }
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
        dplyr::rename(database = cdm_name,
                      n_events = outcome_count,
                      n_persons = denominator_count)
    },
    initPickers = function() {
      # cdm
      private$.cdmPicker <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(inputId = "cdm", label = "Database", choices = unique(private$.data$database), selected = unique(private$.data$database), multiple = TRUE,
                               options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.cdmPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.cdmPicker)

      # outcome
      private$.outcomePicker <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(inputId = "outcome", label = "Outcome", choices = unique(private$.data$outcome_cohort_name), selected = unique(private$.data$outcome_cohort_name), multiple = TRUE,
                                   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.outcomePicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.outcomePicker)

      # denominator age group
      private$.denomAgeGroupPicker <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(inputId = "age_group", label = "Age group", choices = unique(private$.data$denominator_age_group), selected = unique(private$.data$denominator_age_group), multiple = TRUE,
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.denomAgeGroupPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.denomAgeGroupPicker)

      # denominator sex
      private$.denomSexPicker <- InputPanel$new(
        funs = list(denom_sex = shinyWidgets::pickerInput),
        args = list(denom_sex = list(inputId = "denom_sex", choices = unique(private$.data$denominator_sex), label = "Sex", selected = unique(private$.data$denominator_sex), multiple = TRUE,
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.denomSexPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.denomSexPicker)

      # prior observation
      private$.denomPriorObsPicker <- InputPanel$new(
        funs = list(prior_obs = shinyWidgets::pickerInput),
        args = list(prior_obs = list(inputId = "prior_obs", choices = unique(private$.data$denominator_days_prior_observation), label = "Prior observation", selected = unique(private$.data$denominator_days_prior_observation), multiple = TRUE,
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.denomPriorObsPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.denomPriorObsPicker)

      # denominator start date
      private$.denomStartDatePicker <- InputPanel$new(
        funs = list(start_date = shinyWidgets::pickerInput),
        args = list(start_date = list(inputId = "start_date", choices = unique(private$.data$denominator_start_date), label = "Start date", selected = unique(private$.data$denominator_start_date), multiple = TRUE,
                                      options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.denomStartDatePicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.denomStartDatePicker)

      # denominator end date
      private$.denomEndDatePicker <- InputPanel$new(
        funs = list(end_date = shinyWidgets::pickerInput),
        args = list(end_date = list(inputId = "end_date", choices = unique(private$.data$denominator_end_date), label = "End date", selected = unique(private$.data$denominator_end_date), multiple = TRUE,
                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.denomEndDatePicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.denomEndDatePicker)

      # washout
      private$.outcomeWashoutPicker <- InputPanel$new(
        funs = list(washout = shinyWidgets::pickerInput),
        args = list(washout = list(inputId = "washout", choices = unique(private$.data$analysis_outcome_washout), label = "Outcome washout", selected = unique(private$.data$analysis_outcome_washout), multiple = TRUE,
                                   options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.outcomeWashoutPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.outcomeWashoutPicker)

      # repeated events
      private$.repeatedEventsPicker <- InputPanel$new(
        funs = list(repeated_events = shinyWidgets::pickerInput),
        args = list(repeated_events = list(inputId = "repeated_events", choices = unique(private$.data$analysis_repeated_events), label = "Repeated events", selected = unique(private$.data$analysis_repeated_events), multiple = TRUE,
                                           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.repeatedEventsPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.repeatedEventsPicker)

      # complete period
      private$.completePeriodPicker <- InputPanel$new(
        funs = list(complete_period = shinyWidgets::pickerInput),
        args = list(complete_period = list(inputId = "complete_period", choices = unique(private$.data$analysis_complete_database_intervals), label = "Complete period", selected = unique(private$.data$analysis_complete_database_intervals), multiple = TRUE,
                                           options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.completePeriodPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.completePeriodPicker)

      # min counts
      private$.minCountsPicker <- InputPanel$new(
        funs = list(min_cell_count = shinyWidgets::pickerInput),
        args = list(min_cell_count = list(inputId = "min_cell_count", choices = unique(private$.data$analysis_min_cell_count), label = "Minimum counts", selected = unique(private$.data$analysis_min_cell_count), multiple = TRUE,
                                          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.minCountsPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.minCountsPicker)

      # interval
      private$.intervalPicker <- InputPanel$new(
        funs = list(interval = shinyWidgets::pickerInput),
        args = list(interval = list(inputId = "interval", choices = unique(private$.data$analysis_interval), label = "Interval", selected = unique(private$.data$analysis_interval)[1], multiple = TRUE,
                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.intervalPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.intervalPicker)

      # start date
      private$.incStartDatePicker <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(inputId = "year", choices = unique(private$.data$incidence_start_date), label = "Year", selected = unique(private$.data$incidence_start_date), multiple = TRUE,
                                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.incStartDatePicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.incStartDatePicker)

      # plot pickers
      plotDataChoices <- c("database", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals",
                           "analysis_min_cell_count", "analysis_interval", "incidence_start_date")
      # x-axis
      private$.xAxisPicker <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(inputId = "xAxis", choices = plotDataChoices, label = "Incidence_start_date", selected = "incidence_start_date", multiple = F,
                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.xAxisPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.xAxisPicker)

      # facet by
      private$.facetByPicker <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name", "database"), multiple = TRUE,
                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.facetByPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.facetByPicker)

      # color by
      private$.colorByPicker <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c(), multiple = TRUE,
                                    options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.colorByPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.colorByPicker)

      # ribbon
      private$.ribbonPicker <- InputPanel$new(
        funs = list(ribbon = shinyWidgets::pickerInput),
        args = list(ribbon = list(inputId = "ribbon", choices = c(TRUE, FALSE), label = "Ribbon", selected = TRUE, multiple = FALSE,
                                  options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.ribbonPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.ribbonPicker)

      # confidence interval
      private$.confIntervalPicker <- InputPanel$new(
        funs = list(conf_interval = shinyWidgets::pickerInput),
        args = list(conf_interval = list(inputId = "conf_interval", choices = c(TRUE, FALSE), label = "Confidence interval", selected = FALSE, multiple = FALSE,
                                         options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"))),
        addDiv = TRUE
      )
      private$.confIntervalPicker$parentNamespace <- self$namespace
      private$.pickers <- append(private$.pickers, private$.confIntervalPicker)
    },
    # Plotting functions are copied from previous version of IncidencePrevalence,
    # since newest version doesn't support options anymore
    plotEstimates = function(result,
                             x,
                             y,
                             ylim,
                             ytype,
                             ribbon,
                             facet,
                             colour,
                             colour_name,
                             options) {
      errorMessage <- checkmate::makeAssertCollection()
      checkmate::assertTRUE(inherits(result, "IncidencePrevalenceResult"))
      checkmate::assertTRUE(all(c(x, y) %in% colnames(result)))
      checkmate::assertList(options, add = errorMessage)
      checkmate::reportAssertions(collection = errorMessage)

      plot_data <- private$getPlotData(
        estimates = result,
        facetVars = facet,
        colourVars = colour
      )

      if (is.null(colour)) {
        plot <- plot_data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !!rlang::sym(x),
              y = !!rlang::sym(y)
            )
          )
      } else {
        plot <- plot_data %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = !!rlang::sym(x),
              y = !!rlang::sym(y),
              group = .data$colour_vars,
              colour = .data$colour_vars,
              fill = .data$colour_vars
            )
          ) +
          ggplot2::geom_point(size = 2.5) +
          ggplot2::labs(
            fill = colour_name,
            colour = colour_name
          )
      }

      hideConfidenceInterval <- "hideConfidenceInterval" %in% names(options) &&
        options[["hideConfidenceInterval"]]
      yLower <- ifelse(hideConfidenceInterval, y, paste0(y, "_95CI_lower"))
      yUpper <- ifelse(hideConfidenceInterval, y, paste0(y, "_95CI_upper"))

      plot <- plot +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = !!rlang::sym(yLower),
            ymax = !!rlang::sym(yUpper)
          ),
          width = 0
        )

      if (is.null(ylim)) {
        if (ytype == "count") {
          plot <- plot +
            ggplot2::scale_y_continuous(labels = scales::comma)
        }
        if (ytype == "percentage") {
          plot <- plot +
            ggplot2::scale_y_continuous(
              labels =
                scales::percent_format(accuracy = 0.1)
            )
        }
      } else {
        plot <- private$addYLimits(plot = plot, ylim = ylim, ytype = ytype)
      }

      if (!is.null(facet)) {
        facetNcols <- NULL
        if ("facetNcols" %in% names(options)) {
          facetNcols <- options[["facetNcols"]]
        }
        plot <- plot +
          ggplot2::facet_wrap(ggplot2::vars(.data$facet_var), ncol = facetNcols) +
          ggplot2::theme_bw()
      } else {
        plot <- plot +
          ggplot2::theme_minimal()
      }
      if (isTRUE(ribbon)) {
        plot <- private$addRibbon(plot = plot, yLower = yLower, yUpper = yUpper)
      }
      return(plot)
    },
    getPlotData = function(estimates, facetVars, colourVars) {
      plotData <- estimates
      if (!is.null(facetVars)) {
        plotData <- plotData %>%
          tidyr::unite("facet_var",
                       c(tidyselect::all_of(.env$facetVars)),
                       remove = FALSE, sep = "; "
          )
      }
      if (!is.null(colourVars)) {
        plotData <- plotData %>%
          tidyr::unite("colour_vars",
                       c(tidyselect::all_of(.env$colourVars)),
                       remove = FALSE, sep = "; "
          )
      }

      return(plotData)
    },
    addYLimits = function(plot, ylim, ytype) {
      if (ytype == "count") {
        plot <- plot +
          ggplot2::scale_y_continuous(
            labels = scales::comma,
            limits = ylim
          )
      }
      if (ytype == "percentage") {
        plot <- plot +
          ggplot2::scale_y_continuous(
            labels =
              scales::percent_format(accuracy = 0.1),
            limits = ylim
          )
      }
      return(plot)
    },
    addRibbon = function(plot, yLower, yUpper) {
      plot <- plot +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = !!rlang::sym(yLower),
            ymax = !!rlang::sym(yUpper)
          ),
          alpha = .3, color = NA, show.legend = FALSE
        ) +
        ggplot2::geom_line(linewidth = 0.25)
    }
  )
)
