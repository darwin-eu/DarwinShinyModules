#' @title Cohort tab
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortTab Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' cohortTab <- CohortTab$new(appId = "id", data = mtcars)
#'
#' if (interactive()) {
#'   preview(cohortTab)
#' }
CohortTab <- R6::R6Class(
  classname = "CohortTab",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #'
    #' @return `self`
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.data <- data
      private$.cohortTable <- Table$new(appId = appId,
                                        data = data,
                                        options = list(scrollX = TRUE, dom = 't'),
                                        filter = 'top')

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
    #' @param tabName (`character(1)`) Title to use for the tab.
    #' @param tableTitle (`character(1)`) Title to use for the cohort table.
    #'
    #' @return `shiny.tag.list`
    UI = function(tabName = "cohort_attrition", tableTitle = "Cohort attrition") {
      tabItem(
        tabName = tabName,
        private$.cohortTable$UI(tableTitle)
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
      promises::future_promise(private$.cohortTable$server(input, output, session))
    }
  ),

  # Active ----
  active = list(
    #' @field cohortTable (`Table`) Module.
    cohortTable = function() return(private$.cohortTable)
  ),

  # Private ----
  private = list(
    .data = NULL,
    .cohortTable = NULL
  )
)
