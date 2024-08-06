#' @title Incidence menu item
#'
#' @include ShinyModule.R
#'
#' @description
#' IncidenceMenuItem Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' incidenceMenuItem <- IncidenceMenuItem$new(appId = "id", data = mtcars)
#'
#' if (interactive()) {
#'   preview(incidenceMenuItem)
#' }
IncidenceMenuItem <- R6::R6Class(
  classname = "IncidenceMenuItem",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Incidence data to display. It should be generated from the IncidencePrevalence package.
    #' @param addTreatmentPicker if a picker needs to be added containing treatment
    #'
    #' @return `self`
    initialize = function(appId, data, addTreatmentPicker = FALSE) {
      super$initialize(appId)
      private$.data <- data
      private$.addTreatmentPicker = addTreatmentPicker

      private$.cdmPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_cdm_name"), data = private$.data$cdm_name, label = "CDM name")
      private$.outcomePicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_outcome_cohort_name"), data = private$.data$outcome_cohort_name, label = "Outcome name")
      private$.strataPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_cohort_name"), data = private$.data$denominator_cohort_name, label = "Strata")
      private$.denomAgeGroupPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_age_group"), data = private$.data$denominator_age_group, label = "Age group")
      private$.denomSexPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_sex"), data = private$.data$denominator_sex, label = "Sex")
      private$.denomPriorObsPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_days_prior_observation"), data = private$.data$denominator_days_prior_observation, label = "Days prior observation")
      private$.denomStartDatePicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_start_date"), private$.data$denominator_start_date, label = "Start date")
      private$.denomEndDatePicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_denominator_end_date"), data = private$.data$denominator_end_date, label = "End date")
      private$.outcomeWashoutPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_analysis_outcome_washout"), data = private$.data$analysis_outcome_washout, label = "Outcome washout")
      private$.repeatedEventsPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_analysis_repeated_events"), data = private$.data$analysis_repeated_events, label = "Repeated events")
      private$.completePeriodPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_analysis_complete_database_intervals"), data = private$.data$analysis_complete_database_intervals, label = "Complete period")
      private$.minCountsPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_analysis_min_cell_count"), data = private$.data$analysis_min_cell_count, label = "Minimum counts")
      private$.intervalPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_analysis_interval"), data = private$.data$analysis_interval, label = "Interval")
      private$.incStartDatePicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_incidence_start_date"), data = private$.data$incidence_start_date, label = "Incidence start date")

      if (addTreatmentPicker) {
        private$.treatmentPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_treatment_name"), data = private$.data$treatment, label = "Treatment name")
      }

      # plot picker
      plotDataChoices <- c("cdm_name", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals",
                           "analysis_min_cell_count", "analysis_interval", "incidence_start_date")
      private$.incXAxisPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_plot_x"),
                                            data = plotDataChoices,
                                            selected = "incidence_start_date",
                                            label = "Incidence start date",
                                            multiple = FALSE)
      private$.incFacetByPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_plot_facet"),
                                              data = plotDataChoices,
                                              selected = c("outcome_cohort_name", "cdm_name"),
                                              label = "Facet by")
      private$.incColorByPicker <- Picker$new(appId = shiny::NS(appId, "incidence_estimates_plot_colour"),
                                              data = plotDataChoices,
                                              selected = c(),
                                              label = "Colour by")
      return(invisible(self))
    },

    #' @description validate
    #'
    #' @return `self`
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "appId",
        x = private$.appId,
        len = 1
      )
      checkmate::assertDataFrame(
        .var.name = "data",
        x = private$.data,
        add = assertions
      )
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

    #' UI
    #'
    #' @param text (`character(1)`) Menu item text.
    #' @param tabName (`character(1)`) Title to use for the tab.
    #' @param subItemText (`character(1)`) Sub menu item text.
    #' @param subItemTabName (`character(1)`) Sub menu item text.
    #'
    #' @return `shiny.tag`
    UI = function(text = "Incidence",
                  tabName = "incidence",
                  subItemText = "incidence estimates",
                  subItemTabName = "incidence_estimates") {
      shinydashboard::menuItem(
        text = text,
        tabName = tabName,
        shinydashboard::menuSubItem(
          text = subItemText,
          tabName = subItemTabName))
    },

    #' tabItem
    #'
    #' @param tabName (`character(1)`) Title to use for the tab.
    #' @param title (`character(1)`) Title to use as heading.
    #' @param addTreatmentPicker if a picker needs to be added containing treatment
    #'
    #' @return `tabItem`
    tabItem = function(tabName = "incidence_estimates", title = "Incidence estimates - general population") {

      shinydashboard::tabItem(
        tabName = tabName,
        shiny::h3(title),
        shiny::p("Incidence estimates are shown below, please select configuration to filter them:"),
        shiny::p("Database and study outcome"),
        private$.cdmPicker$UI(),
        if (private$.addTreatmentPicker) {
          private$.treatmentPicker$UI()
        },
        private$.outcomePicker$UI(),
        p("Denominator population settings"),
        private$.strataPicker$UI(),
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
          id = shiny::NS(private$.appId, "tabsetPanel"),
          type = "tabs",
          shiny::tabPanel(
            "Table of estimates",
            shiny::downloadButton(shiny::NS(private$.appId, "incidence_estimates_download_table"), "Download current estimates"),
            DT::DTOutput(shiny::NS(private$.appId, "incidence_estimates_table")) %>% withSpinner()
          ),
          shiny::tabPanel(
            "Plot of estimates",
            p("Plotting options"),
            private$.incXAxisPicker$UI(),
            private$.incFacetByPicker$UI(),
            private$.incColorByPicker$UI(),
            plotly::plotlyOutput(
              shiny::NS(private$.appId, "incidence_estimates_plot"),
              height = "800px"
            ) %>%
              shinycssloaders::withSpinner(),
            shiny::h4("Download figure"),
            shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            shiny::div(
              style = "display: inline-block;",
              shiny::textInput(shiny::NS(private$.appId, "incidence_estimates_download_height"), "", 10, width = "50px")
            ),
            shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
            shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            shiny::div(
              style = "display: inline-block;",
              shiny::textInput(shiny::NS(private$.appId, "incidence_estimates_download_width"), "", 20, width = "50px")
            ),
            shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
            shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            shiny::div(
              style = "display: inline-block; margin-right:",
              shiny::textInput(shiny::NS(private$.appId, "incidence_estimates_download_dpi"), "", 300, width = "50px")
            ),
            shiny::downloadButton(shiny::NS(private$.appId, "incidence_estimates_download_plot"), "Download plot")
          )
        )
      )
    },

    #' server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
      getIncidenceEstimates <- reactive({
        result <- private$.data %>%
          dplyr::filter(cdm_name %in% input$incidence_estimates_cdm_name) %>%
          dplyr::filter(outcome_cohort_name %in% input$incidence_estimates_outcome_cohort_name) %>%
          dplyr::filter(denominator_cohort_name %in% input$incidence_estimates_denominator_cohort_name) %>%
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
            incidence_1000_pys = round(suppressWarnings(as.numeric(incidence_1000_pys))),
            incidence_1000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_1000_pys_95CI_lower))),
            incidence_1000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_1000_pys_95CI_upper)))
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
      output$incidence_estimates_table <- renderDataTable({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))

        table <- table %>%
          mutate(incidence_1000_pys = paste0(
            incidence_1000_pys, " (", incidence_1000_pys_95CI_lower, " to ",
            incidence_1000_pys_95CI_upper, " )"
          ))

        if ("treatment" %in% colnames(table)) {
          table <- table %>%
            select(cdm_name, treatment, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_1000_pys)
        } else {
          table <- table %>%
            select(cdm_name, outcome_cohort_name, denominator_cohort_name, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, analysis_outcome_washout, analysis_repeated_events, analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, incidence_start_date, n_events, n_persons, person_years, incidence_1000_pys)
        }

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
        IncidencePrevalence:::plotEstimates(
          result = table,
          x = input$incidence_estimates_plot_x,
          y = "incidence_1000_pys",
          ylim = c(0, NA),
          ytype = "count",
          ribbon = TRUE,
          facet = input$incidence_estimates_plot_facet,
          colour = input$incidence_estimates_plot_colour,
          colour_name = paste0(input$incidence_estimates_plot_colour, collapse = "; "),
          options = list()
        ) +
          ggplot2::scale_fill_discrete(breaks = ageGroups) +
          ggplot2::scale_color_discrete(breaks = ageGroups)

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
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .addTreatmentPicker = FALSE,
    .cdmPicker = NULL,
    .strataPicker = NULL,
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
    .incColorByPicker = NULL
  )
)
