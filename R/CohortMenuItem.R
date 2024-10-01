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

  # Active ----
  active = list(
    #' @param text (`character(1)`) Menu item text.
    text = function(text) {
      if (missing(text)) {
        return(private$.text)
      } else {
        checkmate::assertCharacter(text, len = 1)
        private$.text <- text
      }
    },

    #' @param subItemText (`character(1)`) Sub menu item text.
    subItemText = function(subItemText) {
      if (missing(subItemText)) {
        return(private$.subItemText)
      } else {
        checkmate::assertCharacter(subItemText, len = 1)
        private$.subItemText <- subItemText
      }
    },

    #' @field cohortTable (`Table`) Module.
    cohortTable = function() return(private$.cohortTable)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param data Data to display in the table, usually a `data.frame`-like object.
    #'
    #' @return `self`
    initialize = function(data, text = "text", subItemText = "subItemText") {
      super$initialize()

      private$.text <- text
      private$.subItemText <- subItemText

      private$.cohortTable <- Table$new(
        data = data,
        options = list(scrollX = TRUE, dom = "t"),
        filter = "top"
      )
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
    #' @return `shiny.tag`
    UI = function() {
      shinydashboard::menuItem(
        text = private$.text,
        tabName = "cohorts",
        shinydashboard::menuSubItem(
          text = private$.subItemText,
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
      shinydashboard::tabItem(
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

  # Private ----
  private = list(
    .text = "",
    .subItemText = "",
    .cohortTable = NULL
  )
)
