tpSummaryEventDuration <- R6::R6Class(
  classname = "SummaryEventDuration",
  inherit = ShinyModule,

  ## Active ----
  active = list(),

  ## Public ----
  public = list(
    initialize = function(summaryEventDuration) {
      super$initialize()
      private$.summaryEventDuration <- summaryEventDuration

      private$.table <- Table$new(summaryEventDuration, title = NULL)
      private$.table$parentNamespace <- self$namespace

      private$.plot <- PlotPlotly$new(
        fun = TreatmentPatterns::plotEventDuration,
        args = list(eventDurations = summaryEventDuration)
      )
      private$.plot$parentNamespace <- self$namespace
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .summaryEventDuration = NULL,

    ### Methods ----
    .UI = function() {

    },

    .server = function(input, output, session) {

    }
  )
)

tpRes <- TreatmentPatterns::TreatmentPatternsResults$new(
  filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/output.zip"
)

summaryEventDuration <- tpRes$summary_event_duration
