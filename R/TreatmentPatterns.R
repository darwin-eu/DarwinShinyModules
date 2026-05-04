TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field analyses Analyses table from TreatmentPatterns
    analyses = function() {
      return(private$.analyses)
    },

    treatment_pathways = function() {
      return(private$.treatment_pathways)
    },

    summary_event_duration = function() {
      return(private$.summary_event_duration)
    },

    counts_age = function() {
      return(private$.counts_age)
    },

    counts_sex = function() {
      return(private$.counts_sex)
    },

    counts_year = function() {
      return(private$.counts_year)
    },

    attrition = function() {
      return(private$.attrition)
    },

    metadata = function() {
      return(private$.metadata)
    },

    arguments = function() {
      return(private$.arguments)
    },

    cdm_source_info = function() {
      return(private$.cdm_source_info)
    }
  ),

  # Public ----
  public = list(
    initialize = function(...) {
      super$initialize(...)
      dots <- list(...)
      private$.parseDots(dots)
      private$.parseTPRS()

      private$.initAnalyses()

      private$.initTreatmentPathways()

      private$.initCountsAge()
      private$.initCountsSex()
      private$.initCountsIndexYear()

      private$.initAttrition()

      private$.initMetadata()
      private$.initArguments()
      private$.initCDMSourceInfo()
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

    .treatmentPathwaysMod = NULL,

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
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpAge"),
            label = "Age",
            choices = unique(tpMod$treatment_pathways$age),
            selected = "all"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpSex"),
            label = "Sex",
            choices = unique(tpMod$treatment_pathways$sex),
            selected = "all"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tpIndexYear"),
            label = "Index Year",
            choices = unique(tpMod$treatment_pathways$index_year),
            selected = "all"
          ),
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
          private$.treatmentPathwaysMod$UI()
        )
      )
    },

    .uiSummaryEventDuration = function() {
      shiny::tagList()
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

      private$.serverTreatmentPathways(input, output, session)

      private$.metadataMod$server(input, output, session)
      private$.argumentsMod$server(input, output, session)
      private$.cdmSourceInfoMod$server(input, output, session)

      private$.serverCountsAge(input, output, session)
      private$.serverCountsSex(input, output, session)
      private$.serverCountsIndexYear(input, output, session)

      private$.serverAttrition(input, output, session)
    },

    .serverTreatmentPathways = function(input, output, session) {
      shiny::observeEvent(list(input$tpAge, input$tpSex, input$tpIndexYear, input$tpMinFreq, input$tpMaxFreq), {
        private$.treatmentPathwaysMod$args$result <- private$.treatment_pathways |>
          dplyr::filter(
            .data$age %in% input$tpAge,
            .data$sex %in% input$tpSex,
            .data$index_year %in% input$tpIndexYear,
            .data$freq >= private$.pathFreqChoices[[input$tpMinFreq]],
            .data$freq <= private$.pathFreqChoices[[input$tpMaxFreq]]
          ) |>
          dplyr::left_join(
            private$.cdm_source_info |>
              dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
            by = c("result_id", "analysis_id")
          ) |>
          dplyr::relocate("cdm_name", "target_cohort_name", "age", "sex", "index_year", "pathway", "freq") |>
          dplyr::select(-"analysis_id", -"target_cohort_id", -"result_id") |>
          dplyr::arrange(.data$cdm_name, .data$target_cohort_name, .data$age, .data$sex, .data$index_year, dplyr::desc(.data$freq))
        private$.treatmentPathwaysMod$server(input, output, session)
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
      private$.pathFreqChoices <- list(
        maximum = max(tpMod$treatment_pathways$freq),
        `99%` = quantile(tpMod$treatment_pathways$freq, probs = 0.99),
        `97.5%` = quantile(tpMod$treatment_pathways$freq, probs = 0.975),
        `95%` = quantile(tpMod$treatment_pathways$freq, probs = 0.95),
        `75%` = quantile(tpMod$treatment_pathways$freq, probs = 0.75),
        median = round(median(tpMod$treatment_pathways$freq)),
        mean = round(mean(tpMod$treatment_pathways$freq)),
        `25%` = quantile(tpMod$treatment_pathways$freq, probs = 0.25),
        `5%` = quantile(tpMod$treatment_pathways$freq, probs = 0.05),
        `0.25%` = quantile(tpMod$treatment_pathways$freq, probs = 0.025),
        `1%` = quantile(tpMod$treatment_pathways$freq, probs = 0.01),
        minimum = min(tpMod$treatment_pathways$freq)
      )

      names(private$.pathFreqChoices) <- sprintf(
        "%s (%s)",
        names(private$.pathFreqChoices), unlist(private$.pathFreqChoices)
      )

      private$.treatmentPathwaysMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = NULL,
          style = "darwin",
          groupColumn = "target_cohort_name",
          type = "flextable"
        ),
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
              dplyr::mutate(n = as.character(n))
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
          tpMod$cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::left_join(tpMod$analyses, by = c("result_id", "analysis_id")) |>
        dplyr::select(-"result_id", -"analysis_id", -"target_cohort_id") |>
        dplyr::relocate("cdm_name", "target_cohort_name", "description", colName, "n")
    }
  )
)
