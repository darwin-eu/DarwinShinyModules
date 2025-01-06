#' @title IncidencePrevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' IncidencePrevalence module that shows a that supports incidence; point and
#' period prevalence results from the IncidencePrevalence package.
#'
#' @details
#' The module consists of the following:
#' \describe{
#'   \item{"PlotPlotly"}{Interactive Plotly plot, visualizing the data.}
#'   \item{"GTTable"}{gttable visualizing the tidy data}
#'   \item{"GTTable"}{gttable visualizing the attirtion data}
#'   \item{"Table"}{basic table visualizing the raw data}
#' }
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(IncidencePrevalence)
#'  library(DarwinShinyModules)
#'
#'  cdm <- mockIncidencePrevalence(sampleSize = 1000)
#'  cdm <- generateDenominatorCohortSet(
#'    cdm = cdm, name = "denominator",
#'    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
#'  )
#'
#'  inc <- estimateIncidence(
#'    cdm = cdm,
#'    denominatorTable = "denominator",
#'    outcomeTable = "outcome"
#'  )
#'
#'  pointPrev <- estimatePointPrevalence(
#'    cdm = cdm,
#'    denominatorTable = "denominator",
#'    outcomeTable = "outcome",
#'    interval = "months"
#'  )
#'
#'  periodPrev <- estimatePeriodPrevalence(
#'    cdm = cdm,
#'    denominatorTable = "denominator",
#'    outcomeTable = "outcome",
#'    interval = "months"
#'  )
#'
#'  incMod <- IncidencePrevalence$new(data = inc)
#'  pointPrevMod <- IncidencePrevalence$new(data = pointPrev)
#'  periodPrevMod <- IncidencePrevalence$new(data = periodPrev)
#'
#'  ui <- shiny::fluidPage(
#'    incMod$UI(),
#'    pointPrevMod$UI(),
#'    periodPrevMod$UI()
#'  )
#'
#'  server <- function(input, output, session) {
#'    incMod$server(input, output, session)
#'    pointPrevMod$server(input, output, session)
#'    periodPrevMod$server(input, output, session)
#'  }
#'
#'  if (interactive()) {
#'    shiny::shinyApp(ui = ui, server = server)
#'  }
#' }
#' }
IncidencePrevalence <- R6::R6Class(
  classname = "IncidencePrevalence",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field data (`summarised_result`) Summarised result object.
    data = function() {
      return(private$.data)
    },

    #' @field dataType (`character(1)`) Assumed data type of the provided data.
    #' One of: `"Incidence"`, `"Point Prevalence"`, or `"Period Prevalence"`
    dataType = function() {
      return(private$.dataType)
    },

    #' @field plotPlotly (`PlotPlotly`) Module.
    plotPlotly = function() {
      return(private$.plot)
    },

    #' @field gtTable (`GTTable`) Module.
    gtTable = function() {
      return(private$.gtTable)
    },

    #' @field gtAttrition (`GTTable`) Module.
    gtAttrition = function() {
      return(private$.attrition)
    },

    #' @field table (`Table`) Module.
    table = function() {
      return(private$.table)
    }
  ),

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
      private$assertIPData(data)
      private$.data <- data
      private$.table <- DarwinShinyModules::Table$new(data = data, title = NULL)
      private$.table$parentNamespace <- self$namespace
      return(invisible(self))
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
        private$.dataType <- "Incidence"
        plotFun <- IncidencePrevalence::plotIncidence
        gtTableFun <- IncidencePrevalence::tablePrevalence
        attritionFun <- IncidencePrevalence::tableIncidenceAttrition
      } else if (all(resSettings$result_type %in% c("prevalence", "prevalence_attrition"))) {
        plotFun <- IncidencePrevalence::plotPrevalence
        gtTableFun <- IncidencePrevalence::tablePrevalence
        attritionFun <- IncidencePrevalence::tablePrevalenceAttrition
        if ("point prevalence" %in% resSettings$analysis_type) {
          private$.dataType <- "Point Prevalence"
        } else if ("period prevalence" %in% resSettings$analysis_type) {
          private$.dataType <- "Period Prevalence"
        } else {
          stop("Cannot assert `Point Prevalence` or `Period Prevalence` result")
        }
      } else {
        stop("Cannot assert `Incidence` or `Prevalence` result")
      }

      private$.plot <- DarwinShinyModules::PlotPlotly$new(
        fun = plotFun,
        args = list(result = data),
        title = private$.dataType
      )
      private$.plot$parentNamespace <- self$namespace

      private$.attrition <- GTTable$new(fun = attritionFun, args = list(result = data))
      private$.attrition$parentNamespace <- self$namespace

      private$.gtTable <- GTTable$new(fun = gtTableFun, args = list(result = data))
      private$.gtTable$parentNamespace <- self$namespace
    }
  )
)
