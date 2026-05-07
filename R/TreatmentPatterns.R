# Copyright 2026 DARWIN EU®
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

#' @title TreatmentPatterns Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' TreatmentPatterns module that shows results from the TreatmentPatterns package.
#'
#' @export
TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field analyses Analyses table from TreatmentPatterns
    analyses = function() {
      return(private$.analyses)
    },

    #' @field treatment_pathways Analyses table from TreatmentPatterns
    treatment_pathways = function() {
      return(private$.treatment_pathways)
    },

    #' @field summary_event_duration Analyses table from TreatmentPatterns
    summary_event_duration = function() {
      return(private$.summary_event_duration)
    },

    #' @field counts_age Analyses table from TreatmentPatterns
    counts_age = function() {
      return(private$.counts_age)
    },

    #' @field counts_sex Analyses table from TreatmentPatterns
    counts_sex = function() {
      return(private$.counts_sex)
    },

    #' @field counts_year Analyses table from TreatmentPatterns
    counts_year = function() {
      return(private$.counts_year)
    },

    #' @field attrition Analyses table from TreatmentPatterns
    attrition = function() {
      return(private$.attrition)
    },

    #' @field metadata Analyses table from TreatmentPatterns
    metadata = function() {
      return(private$.metadata)
    },

    #' @field arguments Analyses table from TreatmentPatterns
    arguments = function() {
      return(private$.arguments)
    },

    #' @field cdm_source_info Analyses table from TreatmentPatterns
    cdm_source_info = function() {
      return(private$.cdm_source_info)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param ... Unnamed TreatmentPatternsResults objects. And Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(...) {
      super$initialize(...)
      dots <- list(...)
      private$.parseDots(dots)
      private$.parseTPRS()

      private$.initAnalyses()

      private$.initTreatmentPathways()
      private$.initTreatmentDuration()

      private$.initCountsAge()
      private$.initCountsSex()
      private$.initCountsIndexYear()

      private$.initAttrition()

      private$.initMetadata()
      private$.initArguments()
      private$.initCDMSourceInfo()
      return(self)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    ### Results ----
    .analyses = NULL,

    .treatment_pathways = NULL,

    .summary_event_duration = NULL,

    .counts_age = NULL,
    .counts_sex = NULL,
    .counts_year = NULL,

    .attrition = NULL,

    .metadata = NULL,
    .arguments = NULL,
    .cdm_source_info = NULL,

    ### Modules ----
    .analysesMod = NULL,

    .treatmentPathwaysTable = NULL,
    .treatmentPathwaysSunburst = NULL,

    .treatmentDurationTable = NULL,
    .treatmentDurationPlot = NULL,

    .argumentsMod = NULL,
    .metadataMod = NULL,
    .cdmSourceInfoMod = NULL,

    .attritionMod = NULL,

    .countsAgeMod = NULL,
    .countsSexMod = NULL,
    .countsIndexYearMod = NULL,

    ### Internal ----
    .tprs = NULL,
    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),
    .pathFreqChoices = list(),

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        private$.uiMainFilters(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Analyses",
            private$.analysesMod$UI()
          ),
          shiny::tabPanel(
            title = "Treatment Pathways",
            private$.uiTreatmentPathways()
          ),
          shiny::tabPanel(
            title = "Summary EventDuration",
            private$.uiSummaryEventDuration()
          ),
          shiny::tabPanel(
            title = "Population Counts",
            private$.uiPopulationCounts()
          ),
          shiny::tabPanel(
            title = "Attrition",
            private$.attritionMod$UI()
          ),
          shiny::tabPanel(
            title = "Metadata",
            private$.metadataMod$UI()
          ),
          shiny::tabPanel(
            title = "Parameterisation",
            private$.argumentsMod$UI()
          ),
          shiny::tabPanel(
            title = "CDM Source Information",
            private$.cdmSourceInfoMod$UI()
          )
        )
      )
    },

    .uiMainFilters = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
            label = "Data Source",
            choices = unique(private$.cdm_source_info$cdm_source_abbreviation),
            selected = unique(private$.cdm_source_info$cdm_source_abbreviation)[1:5],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "targetCohort"),
            label = "Target Cohort",
            choices = unique(private$.treatment_pathways$target_cohort_name),
            selected = unique(private$.treatment_pathways$target_cohort_name)[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "analysis"),
            label = "Analysis",
            choices = unique(private$.analyses$description),
            selected = unique(private$.analyses$description)[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .uiTreatmentPathways = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          # TODO Make strata options dynamic
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpAge"),
            label = "Age",
            choices = unique(private$.treatment_pathways$age),
            selected = "all",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpSex"),
            label = "Sex",
            choices = unique(private$.treatment_pathways$sex),
            selected = "all",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpIndexYear"),
            label = "Index Year",
            choices = unique(private$.treatment_pathways$index_year),
            selected = "all",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          # TODO Make choice options dynamic based on reactive data
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpMinFreq"),
            label = "Minimum Frequency",
            choices = names(private$.pathFreqChoices),
            selected = names(private$.pathFreqChoices)[2]
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpMaxFreq"),
            label = "Maximum Frequency",
            choices = names(private$.pathFreqChoices),
            selected = names(private$.pathFreqChoices)[1]
          )
        ),
        shiny::column(
          width = 10,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Table",
              private$.treatmentPathwaysTable$UI()
            ),
            shiny::tabPanel(
              title = "Sunburst Plot",
              shiny::div(
                style = "display: inline-block;",
                shinyWidgets::pickerInput(
                  inputId = shiny::NS(self$namespace, "tpFacetX"),
                  label = "Horizontal Facet",
                  choices = c("cdm_name", "target_cohort_name", "description", "age", "sex", "index_year"),
                  selected = "cdm_name",
                  multiple = TRUE,
                  options = private$.pickerOptions
                )
              ),
              shiny::div(
                style = "display: inline-block;",
                shinyWidgets::pickerInput(
                  inputId = shiny::NS(self$namespace, "tpFacetY"),
                  label = "Vertical Facet",
                  choices = c("cdm_name", "target_cohort_name", "description", "age", "sex", "index_year"),
                  multiple = TRUE,
                  options = private$.pickerOptions
                )
              ),
              private$.treatmentPathwaysSunburst$UI()
            )
          )
        )
      )
    },

    .uiSummaryEventDuration = function() {
      lineChoices <- private$.summary_event_duration |>
        dplyr::filter(.data$line != "overall") |>
        dplyr::distinct(.data$line) |>
        dplyr::pull()

      shiny::tagList(
        shiny::column(
          width = 2,
          # TODO: apply filters to table and plot
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "treatmentGroups"),
            label = "Treatment Groups",
            choices = c("both", "group", "individual"),
            selected = "group",
          ),
          # TODO: update lines to be dynmaic also convert to int, remove overall
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "eventLines"),
            label = "Event Lines",
            choices = c(0, lineChoices),
            selected = 0,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "includeOverall"),
            label = "Include Overall",
            choices = c("Yes", "No"),
            selected = "Yes"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "logXAxis"),
            label = "Log Transform Durations",
            choices = c("Yes", "No"),
            selected = "No"
          )
          # TODO: add min and max duration options
        ),
        shiny::column(
          width = 10,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Table",
              private$.treatmentDurationTable$UI()
            ),
            shiny::tabPanel(
              title = "Plot",
              private$.treatmentDurationPlot$UI()
            )
          )
        )
      )
    },

    .uiPopulationCounts = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Age",
            shinyWidgets::pickerInput(
              inputId = shiny::NS(self$namespace, "countsAge"),
              label = "Age",
              choices = unique(private$.counts_age$age),
              selected = unique(private$.counts_age$age)[1:5],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            private$.countsAgeMod$UI()
          ),
          shiny::tabPanel(
            title = "Sex",
            shinyWidgets::pickerInput(
              inputId = shiny::NS(self$namespace, "countsSex"),
              label = "Sex",
              choices = unique(private$.counts_sex$sex),
              selected = unique(private$.counts_sex$sex)[1:5],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            private$.countsSexMod$UI()
          ),
          shiny::tabPanel(
            title = "Index Year",
            shinyWidgets::pickerInput(
              inputId = shiny::NS(self$namespace, "countsYear"),
              label = "Year",
              choices = unique(private$.counts_year$index_year),
              selected = unique(private$.counts_year$index_year)[5],
              multiple = TRUE,
              options = private$.pickerOptions
            ),
            private$.countsIndexYearMod$UI()
          )
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.analysesMod$server(input, output, session)

      private$.updateTreatmentPathwaysStrata(input, output, session)
      # private$.updateTreatmentPathwaysFreq(input, output, session)

      private$.serverTreatmentPathways(input, output, session)
      private$.serverTreatmentDuration(input, output, session)

      private$.metadataMod$server(input, output, session)
      private$.argumentsMod$server(input, output, session)
      private$.cdmSourceInfoMod$server(input, output, session)

      private$.serverCountsAge(input, output, session)
      private$.serverCountsSex(input, output, session)
      private$.serverCountsIndexYear(input, output, session)

      private$.serverAttrition(input, output, session)
    },

    .updateTreatmentPathwaysStrata = function(input, output, session) {
      baseResult <- private$.reactiveTreatmentPathwaysBase(input)

      shiny::observe({
        res <- baseResult()

        inputIdMap <- list(
          tpAge = "age",
          tpSex = "sex",
          tpIndexYear = "index_year"
        )

        for (i in seq_len(length(inputIdMap))) {
          inputId <- names(inputIdMap[i])
          col <- inputIdMap[[inputId]]

          shinyWidgets::updatePickerInput(
            session = session,
            inputId = inputId,
            choices = unique(res[[col]])
          )
        }
        private$.treatmentPathwaysTable$server(input, output, session)
      })
    },

    .reactiveTreatmentPathwaysBase = function(input) {
      shiny::reactive({
        private$.treatment_pathways |>
          dplyr::left_join(
            private$.cdm_source_info |>
              dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
            by = c("result_id", "analysis_id")
          ) |>
          dplyr::left_join(private$.analyses, by = c("result_id", "analysis_id")) |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis,
            .data$freq >= private$.pathFreqChoices[[input$tpMinFreq]],
            .data$freq <= private$.pathFreqChoices[[input$tpMaxFreq]]
          ) |>
          dplyr::relocate("cdm_name", "target_cohort_name", "description", "age", "sex", "index_year", "pathway", "freq") |>
          dplyr::select(-"analysis_id", -"target_cohort_id", -"result_id") |>
          dplyr::arrange(.data$cdm_name, .data$target_cohort_name, .data$description, .data$age, .data$sex, .data$index_year, dplyr::desc(.data$freq))
      })
    },

    .reactiveTreatmentPathwaysStrata = function(input, baseResult) {
      shiny::reactive({
        baseResult() |>
          dplyr::filter(
            .data$age %in% nullToDefault(input$tpAge, "all"),
            .data$sex %in% nullToDefault(input$tpSex, "all"),
            .data$index_year %in% nullToDefault(input$tpIndexYear, "all")
          )
      })
    },

    .updateTreatmentPathwaysFreq = function(input, output, session) {
      baseResult <- baseResult <- private$.reactiveTreatmentPathwaysBase(input) |>
        private$.reactiveTreatmentPathwaysStrata(input = input)

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "tpMaxFreq",
        choices = names(private$.pathFreqChoices),
        selected = names(private$.pathFreqChoices)[1]
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "tpMinFreq",
        choices = names(private$.pathFreqChoices),
        selected = names(private$.pathFreqChoices)[2]
      )

      shiny::observe({
        result <- baseResult()
        private$.pathFreqChoices <- getFreqRanges(result)

        tpMinFreq <- input$tpMinFreq |>
          nullToDefault(default = names(private$.pathFreqChoices[2]))

        tpMaxFreq <- input$tpMaxFreq |>
          nullToDefault(default = names(private$.pathFreqChoices[1]))

        browser()

        private$.treatmentPathwaysTable$args$result <- result |>
          dplyr::filter(
            .data$freq >= private$.pathFreqChoices[[tpMinFreq]],
            .data$freq <= private$.pathFreqChoices[[tpMaxFreq]]
          )

        private$.treatmentPathwaysTable$server(input, output, session)
      })
    },

    .serverTreatmentPathways = function(input, output, session) {
      result <- private$.reactiveTreatmentPathwaysBase(input) |>
        private$.reactiveTreatmentPathwaysStrata(input = input)
      shiny::observe({
        private$.treatmentPathwaysTable$args$result <- result() |>
          dplyr::group_by(.data$cdm_name, .data$target_cohort_name, .data$description) |>
          dplyr::mutate(
            `%` = round(.data$freq / sum(.data$freq) * 100, 2)
          )
        private$.treatmentPathwaysTable$server(input, output, session)
      })

      shiny::observe({
        private$.treatmentPathwaysSunburst$args$treatmentPathways <- result()
        private$.treatmentPathwaysSunburst$args$strataX <- input$tpFacetX
        private$.treatmentPathwaysSunburst$args$strataY <- input$tpFacetY
        private$.treatmentPathwaysSunburst$server(input, output, session)
      })
    },

    .getReactiveTreatmentDuration = function(input) {
      shiny::reactive({
        private$.summary_event_duration |>
          dplyr::mutate(
            duration_average = round(.data$duration_average, 2),
            duration_sd = round(.data$duration_sd, 2)
          ) |>
          dplyr::left_join(
            private$.cdm_source_info |>
              dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
            by = c("result_id", "analysis_id")
          ) |>
          dplyr::left_join(private$.analyses, by = c("result_id", "analysis_id")) |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis
          ) |>
          dplyr::select(-"analysis_id", -"target_cohort_id", -"result_id") |>
          dplyr::relocate("cdm_name", "target_cohort_name", "description", "line")
      })
    },

    .serverTreatmentDuration = function(input, output, session) {
      result <- private$.getReactiveTreatmentDuration(input)

      shiny::observe({
        private$.treatmentDurationPlot$args$treatmentGroups <- input$treatmentGroups
        private$.treatmentDurationPlot$args$eventLines <- input$eventLines
        private$.treatmentDurationPlot$args$includeOverall <- convertLabelToLogical(
          input$includeOverall,
          trueVal = "Yes",
          falseVal = "No"
        )

        logXAxis <- convertLabelToLogical(input$logXAxis, trueVal = "Yes", falseVal = "No")

        res <- result() |>
          dplyr::group_by(.data$cdm_name, .data$target_cohort_name, .data$description) |>
          dplyr::mutate(
            `%` = round(.data$event_count / sum(.data$event_count) * 100, 2)
          )

        if (logXAxis) {
          res <- res |>
            dplyr::mutate(
              duration_min = round(log(.data$duration_min), 2),
              duration_q1 = round(log(.data$duration_q1), 2),
              duration_median = round(log(.data$duration_median), 2),
              duration_q2 = round(log(.data$duration_q2), 2),
              duration_max = round(log(.data$duration_max), 2),
              duration_average = round(log(.data$duration_average), 2),
              duration_sd = round(log(.data$duration_sd), 2)
            )

          private$.treatmentDurationPlot$args$xLab <- "log(days)"
          private$.treatmentDurationTable$args$result <- res
          private$.treatmentDurationPlot$args$eventDurations <- res
        } else {
          private$.treatmentDurationPlot$args$xLab <- "days"
          private$.treatmentDurationTable$args$result <- res
          private$.treatmentDurationPlot$args$eventDurations <- res
        }


        private$.treatmentDurationTable$server(input, output, session)
        private$.treatmentDurationPlot$server(input, output, session)
      })
    },

    .serverCountsAge = function(input, output, session) {
      shiny::observeEvent(list(input$countsAge, input$cdmName, input$targetCohort, input$analysis), {
        private$.countsAgeMod$args$result <- private$.parseCounts(private$.counts_age, "age") |>
          dplyr::filter(
            .data$age %in% input$countsAge,
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis
          )
        private$.countsAgeMod$server(input, output, session)
      })
    },

    .serverCountsSex = function(input, output, session) {
      shiny::observeEvent(list(input$countsSex, input$cdmName, input$targetCohort, input$analysis), {
        private$.countsSexMod$args$result <- private$.parseCounts(private$.counts_sex, "sex") |>
          dplyr::filter(
            .data$sex %in% input$countsSex,
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis
          )
        private$.countsSexMod$server(input, output, session)
      })
    },

    .serverCountsIndexYear = function(input, output, session) {
      shiny::observeEvent(list(input$countsYear, input$cdmName, input$targetCohort, input$analysis), {
        private$.countsIndexYearMod$args$result <- private$.parseCounts(private$.counts_year, "index_year") |>
          dplyr::filter(
            .data$index_year %in% input$countsYear,
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis
          )
        private$.countsIndexYearMod$server(input, output, session)
      })
    },

    .serverAttrition = function(input, output, session) {
      shiny::observeEvent(list(input$cdmName, input$targetCohort, input$analysis), {
        private$.attritionMod$args$result <- private$.attrition |>
          dplyr::left_join(
            private$.cdm_source_info |>
              dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
            by = c("result_id", "analysis_id")
          ) |>
          dplyr::left_join(private$.analyses, by = c("result_id", "analysis_id")) |>
          dplyr::select(-"analysis_id", -"result_id", -"target_cohort_id") |>
          dplyr::relocate("cdm_name", "target_cohort_name", "description", "reason_id", "reason", "number_records", "number_subjects", "time_stamp") |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$target_cohort_name %in% input$targetCohort,
            .data$description %in% input$analysis
          )
        private$.attritionMod$server(input, output, session)
      })
    },

    ## Initializers ----
    .initTreatmentPathways = function() {
      private$.pathFreqChoices <- private$.treatment_pathways |>
        dplyr::filter(.data$age == "all", .data$sex == "all", .data$index_year == "all") |>
        getFreqRanges()

      private$.treatmentPathwaysTable <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          groupColumn = "target_cohort_name",
          type = "flextable",
          rename = c(
            "Data Source" = "cdm_name",
            "Analysis" = "description",
            "N" = "freq"
          )
        ),
        parentNamespace = self$namespace
      )

      private$.treatmentPathwaysSunburst <- PlotStatic$new(
        fun = ggSunburst,
        args = list(style = "darwin"),
        height = "60vw",
        parentNamespace = self$namespace
      )
    },

    .initTreatmentDuration = function() {
      private$.treatmentDurationTable <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          style = "darwin",
          groupColumn = "target_cohort_name",
          type = "flextable",
          rename = c(
            "Data Source" = "cdm_name",
            "Analysis" = "description",
            "Event Line" = "line",
            "Min" = "duration_min",
            "Q25" = "duration_q1",
            "Median" = "duration_median",
            "Q75" = "duration_q2",
            "Max" = "duration_max",
            "Mean" = "duration_average",
            "St Dev" = "duration_sd",
            "N" = "event_count"
          )
        ),
        parentNamespace = self$namespace
      )

      private$.treatmentDurationPlot <- PlotStatic$new(
        fun = plotShinyEventDuration,
        args = list(),
        height = "80vh",
        parentNamespace = self$namespace
      )
    },

    .initAnalyses = function() {
      tbl <- private$.analyses |>
        dplyr::left_join(
          private$.cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::left_join(
          private$.treatment_pathways |>
            dplyr::select("target_cohort_name", "result_id", "analysis_id") |>
            dplyr::distinct(),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::select(-"result_id") |>
        dplyr::relocate("cdm_name", "analysis_id", "description")

      private$.analysesMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = tbl,
          groupColumn = "target_cohort_name",
          type = "flextable",
          style = "darwin"
        ),
        parentNamespace = self$namespace
      )
    },

    .initMetadata = function() {
      metadata <- private$.metadata |>
        dplyr::left_join(
          private$.cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::select(-"result_id") |>
        dplyr::mutate(
          execution_start = as.character(as.POSIXct(as.numeric(.data$execution_start))),
          execution_end = as.character(as.POSIXct(as.numeric(.data$execution_end)))
        ) |>
        dplyr::relocate(
          "analysis_id", "execution_start", "execution_end"
        )

      private$.metadataMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = metadata,
          style = "darwin",
          groupColumn = "cdm_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initArguments = function() {
      arguments <- private$.arguments |>
        dplyr::left_join(
          private$.cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::select(-"result_id")

      private$.argumentsMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = arguments,
          style = "darwin",
          groupColumn = "cdm_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initCDMSourceInfo = function() {
      cdmSourceInfo <- private$.cdm_source_info |>
        dplyr::select(-"analysis_id", -"result_id") |>
        dplyr::distinct()

      private$.cdmSourceInfoMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = cdmSourceInfo,
          style = "darwin",
          groupColumn = "cdm_source_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initCountsAge = function() {
      private$.countsAgeMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          estimateName = "n",
          groupColumn = "target_cohort_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initCountsSex = function() {
      private$.countsSexMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          estimateName = "n",
          groupColumn = "target_cohort_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initCountsIndexYear = function() {
      private$.countsIndexYearMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          estimateName = "n",
          groupColumn = "target_cohort_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    .initAttrition = function() {
      private$.attritionMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          estimateName = "n",
          groupColumn = "target_cohort_name",
          type = "flextable"
        ),
        parentNamespace = self$namespace
      )
    },

    ## Helpers ----
    .parseDots = function(dots) {
      # Unnamed -> TPR
      private$.tprs <- lapply(dots, function(arg) {
        if ("TreatmentPatternsResults" %in% class(arg)) {
          arg
        }
      })

      # Rest parse
      # restArgs <- dots[!names(dots) %in% ""]
      # for (arg in restArgs) {
      #   label <- names(arg)
      #   private[[label]]
      # }
    },

    .parseTPRS = function() {
      for (i in seq_len(length(private$.tprs))) {
        tpr <- private$.tprs[[i]]

        results <- c(
          "analyses", "treatment_pathways", "summary_event_duration",
          "counts_age", "counts_sex", "counts_year", "attrition", "metadata",
          "arguments", "cdm_source_info"
        )

        for (result in results) {
          tbl <- if (result %in% c("counts_age", "counts_sex", "counts_year")) {
            tpr[[result]] |>
              dplyr::mutate(n = as.character(.data$n))
          } else if (result == "cdm_source_info") {
            tpr[[result]] |>
              dplyr::mutate(
                cdm_version = as.character(.data$cdm_version)
              )
          } else {
            tpr[[result]]
          }

          private[[sprintf(".%s", result)]] <- tbl |>
            dplyr::mutate(result_id = i) |>
            dplyr::bind_rows(private[[sprintf(".%s", result)]])
        }
      }
    },

    .parseCounts = function(counts, colName) {
      counts |>
        dplyr::left_join(
          private$.cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::left_join(private$.analyses, by = c("result_id", "analysis_id")) |>
        dplyr::select(-"result_id", -"analysis_id", -"target_cohort_id") |>
        dplyr::relocate("cdm_name", "target_cohort_name", "description", colName, "n")
    }
  )
)
