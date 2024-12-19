IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(),

  # Public ----
  public = list(
    initialize = function(data) {
      super$initialize()
      private$assertIPData(data)
      private$.data <- data
      private$.table <- DarwinShinyModules::Table$new(data = data, title = NULL)
      private$.table$parentNamespace <- self$namespace
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .dataType = "",
    .plot = NULL,
    .gtTable = NULL,
    .attrition = NULL,
    .table = NULL,

    .UI = function() {
      shiny::wellPanel(
        private$.plot$UI(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Tidy Data",
            private$.gtTable$UI()
          ),
          shiny::tabPanel(
            title = "Attrition",
            private$.attrition$UI()
          ),
          shiny::tabPanel(
            title = "Raw Data",
            private$.table$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.plot$server(input, output, session)
      private$.table$server(input, output, session)
      private$.gtTable$server(input, output, session)
      private$.attrition$server(input, output, session)
    },

    assertIPData = function(data) {
      resSettings <- attr(data, "settings")

      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }

      if (all(resSettings$result_type %in% c("incidence", "incidence_attrition"))) {
        dataType <- "Incidence"
        plotFun <- IncidencePrevalence::plotIncidence
        gtTableFun <- IncidencePrevalence::tablePrevalence
        attritionFun <- IncidencePrevalence::tableIncidenceAttrition
      } else if (all(resSettings$result_type %in% c("prevalence", "prevalence_attrition"))) {
        plotFun <- IncidencePrevalence::plotPrevalence
        gtTableFun <- IncidencePrevalence::tablePrevalence
        attritionFun <- IncidencePrevalence::tablePrevalenceAttrition
        if ("point prevalence" %in% resSettings$analysis_type) {
          dataType <- "Point Prevalence"
        } else if ("period prevalence" %in% resSettings$analysis_type) {
          dataType <- "Period Prevalence"
        } else {
          stop("Cannot assert `Point Prevalence` or `Period Prevalence` result")
        }
      } else {
        stop("Cannot assert `Incidence` or `Prevalence` result")
      }

      private$.plot <- DarwinShinyModules::PlotPlotly$new(
        fun = plotFun,
        args = list(result = data),
        title = dataType
      )
      private$.plot$parentNamespace <- self$namespace

      private$.attrition <- GTTable$new(fun = attritionFun, args = list(result = data))
      private$.attrition$parentNamespace <- self$namespace

      private$.gtTable <- GTTable$new(fun = gtTableFun, args = list(result = data))
      private$.gtTable$parentNamespace <- self$namespace
    }
  )
)
