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
    #' @param data Data to display, usually a `data.frame`-like object.
    #'
    #' @return `self`
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.data <- data
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
      menuItem(
        text = text,
        tabName = tabName,
        menuSubItem(
          text = subItemText,
          tabName = subItemTabName
        )
      )
    },

    #' tabItem
    #'
    #' @param tabName (`character(1)`) Title to use for the tab.
    #' @param title (`character(1)`) Title to use as heading.
    #' @param addTreatmentPicker if a picker needs to be added containing treatment
    #'
    #' @return `tabItem`
    tabItem = function(tabName = "incidence_estimates", title = "Incidence estimates - general population", addTreatmentPicker = FALSE) {
      tabItem(
        tabName = tabName,
        h3(title),
        p("Incidence estimates are shown below, please select configuration to filter them:"),
        p("Database and study outcome"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_cdm_name"),
            label = "CDM name",
            choices = unique(private$.data$cdm_name),
            selected = unique(private$.data$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        if (addTreatmentPicker) {
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(
              inputId = shiny::NS(private$.appId, "incidence_estimates_treatment_name"),
              label = "Treatment name",
              choices = unique(private$.data$treatment),
              selected = unique(private$.data$treatment),
              options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
              multiple = TRUE
            )
          )
        },
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_outcome_cohort_name"),
            label = "Outcome name",
            choices = unique(private$.data$outcome_cohort_name),
            selected = unique(private$.data$outcome_cohort_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        p("Denominator population settings"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_cohort_name"),
            label = "Strata",
            choices = unique(private$.data$denominator_cohort_name),
            selected = unique(private$.data$denominator_cohort_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_age_group"),
            label = "Age group",
            choices = unique(private$.data$denominator_age_group),
            selected = unique(private$.data$denominator_age_group),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_sex"),
            label = "Sex",
            choices = unique(private$.data$denominator_sex),
            selected = unique(private$.data$denominator_sex),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_days_prior_observation"),
            label = "Days prior observation",
            choices = unique(private$.data$denominator_days_prior_observation),
            selected = unique(private$.data$denominator_days_prior_observation),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_start_date"),
            label = "Start date",
            choices = unique(private$.data$denominator_start_date),
            selected = unique(private$.data$denominator_start_date),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_denominator_end_date"),
            label = "End date",
            choices = unique(private$.data$denominator_end_date),
            selected = unique(private$.data$denominator_end_date),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        p("Analysis settings"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_analysis_outcome_washout"),
            label = "Outcome washout",
            choices = unique(private$.data$analysis_outcome_washout),
            selected = unique(private$.data$analysis_outcome_washout),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_analysis_repeated_events"),
            label = "Repeated events",
            choices = unique(private$.data$analysis_repeated_events),
            selected = unique(private$.data$analysis_repeated_events),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_analysis_complete_database_intervals"),
            label = "Complete period",
            choices = unique(private$.data$analysis_complete_database_intervals),
            selected = unique(private$.data$analysis_complete_database_intervals),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId,"incidence_estimates_analysis_min_cell_count"),
            label = "Minimum counts",
            choices = unique(private$.data$analysis_min_cell_count),
            selected = unique(private$.data$analysis_min_cell_count),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        p("Dates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_analysis_interval"),
            label = "Interval",
            choices = unique(private$.data$analysis_interval),
            selected = unique(private$.data$analysis_interval),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = shiny::NS(private$.appId, "incidence_estimates_incidence_start_date"),
            label = "Incidence start date",
            choices = unique(private$.data$incidence_start_date),
            selected = unique(private$.data$incidence_start_date),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        tabsetPanel(
          id = shiny::NS(private$.appId, "tabsetPanel"),
          type = "tabs",
          tabPanel(
            "Table of estimates",
            downloadButton(shiny::NS(private$.appId, "incidence_estimates_download_table"), "Download current estimates"),
            DTOutput(shiny::NS(private$.appId, "incidence_estimates_table")) %>% withSpinner()
          ),
          tabPanel(
            "Plot of estimates",
            p("Plotting options"),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = shiny::NS(private$.appId, "incidence_estimates_plot_x"),
                label = "x axis",
                choices = c("cdm_name", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
                selected = "incidence_start_date",
                list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = shiny::NS(private$.appId, "incidence_estimates_plot_facet"),
                label = "Facet by",
                choices = c("cdm_name", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
                selected = c("outcome_cohort_name", "cdm_name"),
                list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = TRUE
              )
            ),
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = shiny::NS(private$.appId, "incidence_estimates_plot_colour"),
                label = "Colour by",
                choices = c("cdm_name", "outcome_cohort_name", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation", "denominator_start_date", "denominator_end_date", "analysis_outcome_washout", "analysis_repeated_events", "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"),
                list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
                multiple = TRUE
              )
            ),
            plotlyOutput(
              shiny::NS(private$.appId, "incidence_estimates_plot"),
              height = "800px"
            ) %>%
              withSpinner(),
            h4("Download figure"),
            div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            div(
              style = "display: inline-block;",
              textInput(shiny::NS(private$.appId, "incidence_estimates_download_height"), "", 10, width = "50px")
            ),
            div("cm", style = "display: inline-block; margin-right: 25px;"),
            div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            div(
              style = "display: inline-block;",
              textInput(shiny::NS(private$.appId, "incidence_estimates_download_width"), "", 20, width = "50px")
            ),
            div("cm", style = "display: inline-block; margin-right: 25px;"),
            div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
            div(
              style = "display: inline-block; margin-right:",
              textInput(shiny::NS(private$.appId, "incidence_estimates_download_dpi"), "", 300, width = "50px")
            ),
            downloadButton(shiny::NS(private$.appId, "incidence_estimates_download_plot"), "Download plot")
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
          filter(cdm_name %in% input$incidence_estimates_cdm_name) %>%
          filter(outcome_cohort_name %in% input$incidence_estimates_outcome_cohort_name) %>%
          filter(denominator_cohort_name %in% input$incidence_estimates_denominator_cohort_name) %>%
          filter(denominator_age_group %in% input$incidence_estimates_denominator_age_group) %>%
          filter(denominator_sex %in% input$incidence_estimates_denominator_sex) %>%
          filter(denominator_days_prior_observation %in% input$incidence_estimates_denominator_days_prior_observation) %>%
          filter(denominator_start_date %in% input$incidence_estimates_denominator_start_date) %>%
          filter(denominator_end_date %in% input$incidence_estimates_denominator_end_date) %>%
          filter(analysis_outcome_washout %in% input$incidence_estimates_analysis_outcome_washout) %>%
          filter(analysis_repeated_events %in% input$incidence_estimates_analysis_repeated_events) %>%
          filter(analysis_complete_database_intervals %in% input$incidence_estimates_analysis_complete_database_intervals) %>%
          filter(analysis_min_cell_count %in% input$incidence_estimates_analysis_min_cell_count) %>%
          filter(analysis_interval %in% input$incidence_estimates_analysis_interval) %>%
          filter(incidence_start_date %in% input$incidence_estimates_incidence_start_date) %>%
          mutate(
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

        datatable(
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
          scale_fill_discrete(breaks = ageGroups) +
          scale_color_discrete(breaks = ageGroups)

      })
      ### download plot ----
      output$incidence_estimates_download_plot <- downloadHandler(
        filename = function() {
          "incidenceEstimatesPlot.png"
        },
        content = function(file) {
          ggsave(
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

  # Active ----
  active = list(),

  # Private ----
  private = list(
    .data = NULL
  )
)
