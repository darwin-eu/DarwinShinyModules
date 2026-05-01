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
    .argumentsMod = NULL,
    .metadataMod = NULL,
    .cdmSourceInfoMod = NULL,

    ### Internal ----
    .tprs = NULL,

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "analyses",
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
            private$.uiAttrition()
          ),
          shiny::tabPanel(
            title = "Metadata",
            private$.uiMetadata()
          )
        )
      )
    },

    .uiTreatmentPathways = function() {
      shiny::tagList()
    },

    .uiSummaryEventDuration = function() {
      shiny::tagList()
    },

    .uiPopulationCounts = function() {
      shiny::tagList()
    },

    .uiAttrition = function() {
      shiny::tagList()
    },

    .uiMetadata = function() {
      shiny::tagList(
        private$.metadataMod$UI(),
        private$.argumentsMod$UI(),
        private$.cdmSourceInfoMod$UI()
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.analysesMod$server(input, output, session)

      private$.metadataMod$server(input, output, session)
      private$.argumentsMod$server(input, output, session)
      private$.cdmSourceInfoMod$server(input, output, session)
    },

    ## Initializers ----
    .initAnalyses = function() {
      tbl <- private$.analyses |>
        dplyr::left_join(
          private$.cdm_source_info |>
            dplyr::select(cdm_name = "cdm_source_abbreviation", "result_id", "analysis_id"),
          by = c("result_id", "analysis_id")
        ) |>
        dplyr::select(-"result_id")

      private$.analysesMod <- Flextable$new(
        fun = visOmopResults::visTable,
        args = list(
          result = tbl,
          groupColumn = "description",
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
    }
  )
)
