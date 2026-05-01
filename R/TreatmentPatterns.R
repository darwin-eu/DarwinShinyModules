TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Active ----
  active = list(),

  # Public ----
  public = list(
    initialize = function(...) {
      super$initialize(...)
      dots <- list(...)
      private$.parseDots(dots)
      private$.parseTPRS()
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    ### Results ----
    analyses = NULL,

    treatment_pathways = NULL,

    summary_event_duration = NULL,

    counts_age = NULL,
    counts_sex = NULL,
    counts_year = NULL,

    attrition = NULL,

    metadata = NULL,
    arguments = NULL,
    cdm_source_info = NULL,

    ### Internal ----
    .tprs = NULL,

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "analyses",
            private$.uiAnalyses()
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
            private$.uiPopulationCounts
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

    .uiAnalyses = function() {
      shiny::tagList()
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
      shiny::tagList()
    },

    ## Server ----
    .server = function() {

    },

    ## Helpers ----
    .parseDots = function(dots) {
      # parentNamespace, async -> super
      dots <- dots[!names(dots) %in% c("parentNamespace", "async")]

      # Unnamed -> TPR
      private$.tprs <- dots[names(dots) %in% ""]

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
          private[[sprintf(".%s", result)]] <- tpr[[result]] |>
            dplyr::mutate(result_id = i) |>
            dplyr::bind_rows(private[[sprintf(".%s", result)]])
        }
      }
    }
  )
)
