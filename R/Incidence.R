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
#'     inc <- readRDS(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/IncidencePrevalence/0.9.0/incidence.rds"
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
      private$.data <- data

      private$.cdmPicker <- Picker$new(data = private$.incData$database, label = "Database")
      private$.outcomePicker <- Picker$new(data = private$.incData$outcome_cohort_name, label = "Outcome name")
      private$.denomAgeGroupPicker <- Picker$new(data = private$.incData$denominator_age_group, label = "Age group", selected = unique(private$.incData$denominator_age_group)[1])
      private$.denomSexPicker <- Picker$new(data = private$.incData$denominator_sex, label = "Sex", selected = unique(private$.incData$denominator_sex)[1])
      private$.denomPriorObsPicker <- Picker$new(data = private$.incData$denominator_days_prior_observation, label = "Days prior observation")
      private$.denomStartDatePicker <- Picker$new(data = private$.incData$denominator_start_date, label = "Start date")
      private$.denomEndDatePicker <- Picker$new(data = private$.incData$denominator_end_date, label = "End date")
      private$.outcomeWashoutPicker <- Picker$new(data = private$.incData$analysis_outcome_washout, label = "Outcome washout")
      private$.repeatedEventsPicker <- Picker$new(data = private$.incData$analysis_repeated_events, label = "Repeated events")
      private$.completePeriodPicker <- Picker$new(data = private$.incData$analysis_complete_database_intervals, label = "Complete period")
      private$.minCountsPicker <- Picker$new(data = private$.incData$analysis_min_cell_count, label = "Minimum counts")
      private$.intervalPicker <- Picker$new(data = private$.incData$analysis_interval, label = "Interval", selected = unique(private$.incData$analysis_interval)[1])
      private$.incStartDatePicker <- Picker$new(data = private$.incData$incidence_start_date, label = "Year")

      # plot picker
      plotDataChoices <- c("database", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals",
                           "analysis_min_cell_count", "analysis_interval", "incidence_start_date")
      private$.incXAxisPicker <- Picker$new(data = plotDataChoices,
                                            label = "Incidence start date",
                                            selected = "incidence_start_date",
                                            multiple = FALSE)
      private$.incFacetByPicker <- Picker$new(data = plotDataChoices,
                                              label = "Facet by",
                                              selected = c("outcome_cohort_name", "database"))
      private$.incColorByPicker <- Picker$new(data = plotDataChoices,
                                              label = "Colour by",
                                              selected = c())
      private$.incRibbonPicker <- Picker$new(data = c(TRUE, FALSE),
                                             label = "Ribbon",
                                             selected = TRUE,
                                             multiple = FALSE)
      private$.incConfIntervalPicker <- Picker$new(data = c(TRUE, FALSE),
                                                   label = "Confidence interval",
                                                   selected = FALSE,
                                                   multiple = FALSE)
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
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
    .incXAxisPicker = NULL,
    .incFacetByPicker = NULL,
    .incColorByPicker = NULL,
    .incRibbonPicker = NULL,
    .incConfIntervalPicker = NULL,

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
              shiny::downloadButton(shiny::NS(private$.namespace, "incidence_estimates_download_table"), "Download current estimates"),
              DT::DTOutput(shiny::NS(private$.namespace, "incidence_estimates_table")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot of estimates",
              p("Plotting options"),
              private$.incXAxisPicker$UI(),
              private$.incFacetByPicker$UI(),
              private$.incColorByPicker$UI(),
              private$.incRibbonPicker$UI(),
              private$.incConfIntervalPicker$UI(),
              plotly::plotlyOutput(
                shiny::NS(private$.namespace, "incidence_estimates_plot"),
                height = "800px"
              ) %>%
                shinycssloaders::withSpinner(),
              shiny::h4("Download figure"),
              shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "incidence_estimates_download_height"), "", 10, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "incidence_estimates_download_width"), "", 20, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block; margin-right:",
                shiny::textInput(shiny::NS(private$.namespace, "incidence_estimates_download_dpi"), "", 300, width = "50px")
              ),
              shiny::downloadButton(shiny::NS(private$.namespace, "incidence_estimates_download_plot"), "Download plot")
            )
          )
        )
      )
    },

    .server = function(input, output, session) {
      # Incidence
      getIncidenceEstimates <- reactive({
        result <- private$.incData %>%
          dplyr::filter(database %in% input$incidence_estimates_database) %>%
          dplyr::filter(outcome_cohort_name %in% input$incidence_estimates_outcome_cohort_name) %>%
          dplyr::filter(denominator_age_group %in% input$incidence_estimates_denominator_age_group) %>%
          dplyr::filter(denominator_sex %in% input$incidence_estimates_denominator_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% input$incidence_estimates_denominator_days_prior_observation) %>%
          dplyr::filter(denominator_start_date %in% input$incidence_estimates_denominator_start_date) %>%
          dplyr::filter(denominator_end_date %in% input$incidence_estimates_denominator_end_date) %>%
          dplyr::filter(analysis_outcome_washout %in% input$incidence_estimates_analysis_outcome_washout) %>%
          dplyr::filter(analysis_repeated_events %in% input$incidence_estimates_analysis_repeated_events) %>%
          dplyr::filter(analysis_complete_database_intervals %in% input$incidence_estimates_analysis_complete_database_intervals) %>%
          dplyr::filter(analysis_min_cell_count %in% input$incidence_estimates_analysis_min_cell_count) %>%
          dplyr::filter(analysis_interval %in% input$incidence_estimates_analysis_interval) %>%
          dplyr::filter(incidence_start_date %in% input$incidence_estimates_incidence_start_date) %>%
          dplyr::mutate(
            person_years = round(suppressWarnings(as.numeric(person_years))),
            person_days = round(suppressWarnings(as.numeric(person_days))),
            n_events = round(suppressWarnings(as.numeric(n_events))),
            incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys))),
            incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower))),
            incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)))
          )
        if ("treatment" %in% colnames(result)) {
          result <- result %>%
            filter(treatment %in% input$incidence_estimates_treatment_name)
        }
        return(result)
      })

      ### download table ----
      output$incidence_estimates_download_table <- downloadHandler(
        filename = function() {
          "incidenceEstimatesTable.csv"
        },
        content = function(file) {
          write_csv(getIncidenceEstimates(), file)
        }
      )

      ### table estimates ----
      output$incidence_estimates_table <- DT::renderDT({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))

        table <- table %>%
          mutate(incidence_100000_pys = paste0(
            incidence_100000_pys, " (", incidence_100000_pys_95CI_lower, " to ",
            incidence_100000_pys_95CI_upper, " )"
          ))

        if ("treatment" %in% colnames(table)) {
          table <- table %>%
            select(database, treatment, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)
        } else {
          table <- table %>%
            select(database, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_100000_pys)
        }

        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })

      #' Adds the given ribbon to the current previewItem string.
      #'
      #' @param previewItemString string representing the previewItem
      #' @param ribbon ribbon value
      #'
      #' @return the updated preview item string
      addPreviewItemRibbon <- function(previewItemString, ribbon) {
        ribbonStr <- paste0("ribbon = ", ribbon)
        return(gsub("ribbon", ribbonStr, previewItemString))
      }

      ### make plot ----
      plotIncidenceEstimates <- reactive({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
        IncidencePrevalence:::plotEstimates(
          result = table,
          x = input$incidence_estimates_plot_x,
          y = "incidence_100000_pys",
          ylim = c(0, NA),
          ytype = "count",
          ribbon = as.logical(input$incidence_estimates_ribbon),
          facet = input$incidence_estimates_plot_facet,
          colour = input$incidence_estimates_plot_colour,
          colour_name = paste0(input$incidence_estimates_plot_colour, collapse = "; "),
          options = list("hideConfidenceInterval" = !as.logical(input$incidence_estimates_confidence_interval))
        ) +
          ggplot2::scale_fill_discrete(breaks = ageGroups) +
          ggplot2::scale_color_discrete(breaks = ageGroups) +
          guides(fill = "none")

      })

      ### download plot ----
      output$incidence_estimates_download_plot <- downloadHandler(
        filename = function() {
          "incidenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            plotIncidenceEstimates(),
            width = as.numeric(input$incidence_estimates_download_width),
            height = as.numeric(input$incidence_estimates_download_height),
            dpi = as.numeric(input$incidence_estimates_download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$incidence_estimates_plot <- renderPlotly({
        plotIncidenceEstimates()
      })

      ## comorbidity ----
      getsummaryComorbidity  <- reactive({
        result <- private$.cmbData %>%
          filter(database %in% input$cmorbs_cdm) %>%
          filter(time_window %in% input$cmorbs_time_window)   %>%
          filter(variable_level %in% input$cmorbs_variable_level) %>%
          filter(strata_level %in% input$cmorbs_strata_level)  %>%
          pivot_wider(names_from = c("database", "time_window"),
                      values_from = c("estimate"),
                      names_glue = "{database} [{time_window}] {.value}",
                      names_vary = "slowest")
        formatObj3Estimates(result)
      })

      output$dt_summaryComorbidity  <- DT::renderDT({
        table_data <- getsummaryComorbidity()
        datatable(table_data, rownames= FALSE)
      })

      output$dt_summaryComorbidity_csv <- downloadHandler(
        filename = function() {
          "summaryComorbidity.csv"
        },
        content = function(file) {
          x <- getsummaryComorbidity()
          write.csv(x,
                    file,
                    row.names=FALSE)
        }
      )

      # medications ----
      getsummaryMedications <- reactive({
        result <- private$.medData %>%
          filter(database %in% input$meds_cdm) %>%
          filter(time_window %in% input$meds_time_window) %>%
          filter(variable_level %in% input$meds_variable_level) %>%
          filter(strata_level %in% input$meds_strata_level)   %>%
          pivot_wider(names_from = c("database", "time_window"),
                      values_from = c("estimate"),
                      names_glue = "{database} [{time_window}] {.value}",
                      names_vary = "slowest")
        formatObj3Estimates(result)
      })

      output$dt_summaryMedications  <- DT::renderDT({
        table_data <- getsummaryMedications()
        datatable(table_data, rownames= FALSE)
      })

      output$dt_summaryMedications_csv <- downloadHandler(
        filename = function() {
          "summaryMedications.csv"
        },
        content = function(file) {
          x <- getsummaryMedications()
          write.csv(x,
                    file,
                    row.names=FALSE)
        }
      )

      # lsc ----
      getsummaryLS <- reactive({
        private$.lscData %>%
          dplyr::filter(database %in% input$ls_cdm) %>%
          dplyr::filter(time_window %in% input$ls_time_window)   %>%
          dplyr::filter(strata_level %in% input$ls_strata_level)  %>%
          dplyr::filter(domain %in% input$ls_domain) %>%
          dplyr::distinct()
      })

      output$dt_summaryLS  <- DT::renderDT({
        table_data <- getsummaryLS() %>%
          pivot_wider(names_from = c("database", "time_window"),
                      values_from = c("estimate"),
                      names_glue = "{database} [{time_window}] {.value}",
                      names_vary = "slowest")
        datatable(formatObj3Estimates(table_data, other = c("variable", "domain")), rownames= FALSE)
      })

      output$dt_summaryLS_csv <- downloadHandler(
        filename = function() {
          "summaryLS.csv"
        },
        content = function(file) {
          x <- getsummaryLS()  %>%
            pivot_wider(names_from = c("database", "time_window"),
                        values_from = c("estimate"),
                        names_glue = "{database} [{time_window}] {.value}",
                        names_vary = "slowest")
          write.csv(x,
                    file,
                    row.names=FALSE)
        }
      )

      getTopLs <- reactive({
        tableData <- getsummaryLS()
        result <- tibble::tibble(rank = 1:input$top_n)

        for (i in seq_along(unique(tableData$database))){
          db <- unique(tableData$database)[i]
          dbData <- tableData %>%
            filter(database == db)

          topData <- dbData %>%
            group_by(strata_level, variable, time_window) %>%
            summarise(n = as.numeric(estimate[1]), percentage = as.numeric(estimate)[2]) %>%
            ungroup() %>%
            slice_max(percentage, n = input$top_n) %>%
            arrange(desc(percentage)) %>%
            mutate(rank = row_number(), !!db :=  paste0(strata_level, " - ", variable, ": ", n, " (", percentage, "%)")) %>%
            select(rank, db)

          result <- result %>%
            left_join(topData,
                      by = "rank")
        }
        result
      })

      output$dt_top  <- DT::renderDT({
        datatable(getTopLs(), rownames= FALSE)
      })

      output$dt_top_csv <- downloadHandler(
        filename = function() {
          "TopLS.csv"
        },
        content = function(file) {
          x <- getTopLs()
          write.csv(x,
                    file,
                    row.names=FALSE)
        }
      )
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
    }
  )
)
