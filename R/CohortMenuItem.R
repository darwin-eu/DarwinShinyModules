#' @title Cohort menu item
#'
#' @include ShinyModule.R
#'
#' @description
#' CohortMenuItem Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' cohortMenuItem <- CohortMenuItem$new(appId = "id", data = mtcars)
#'
#' if (interactive()) {
#'   preview(cohortMenuItem)
#' }
CohortMenuItem <- R6::R6Class(
  classname = "CohortMenuItem",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to display in the table, usually a `data.frame`-like object.
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
    #' @param text (`character(1)`) Menu item text.
    #' @param subItemText (`character(1)`) Sub menu item text.
    #'
    #' @return `shiny.tag`
    UI = function(text = "Cohorts",
                  subItemText = "Cohort attrition") {
      menuItem(
        text = text,
        tabName = "cohorts",
        menuSubItem(
          text = subItemText,
          tabName = "cohort_attrition"
        )
      )
    },

    #' tabItem
    #'
    #' @param tableTitle (`character(1)`) Title to use for the cohort table.
    #'
    #' @return `tabItem`
    tabItem = function(tableTitle = "Cohort attrition") {
      tabItem(
        tabName = "cohort_attrition",
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
