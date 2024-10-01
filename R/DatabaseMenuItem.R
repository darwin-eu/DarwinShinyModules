#' @title Database menu item
#'
#' @include ShinyModule.R
#'
#' @description
#' DatabaseMenuItem Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' DatabaseMenuItem <- DatabaseMenuItem$new(data = mtcars)
#'
#' if (interactive()) {
#'   preview(DatabaseMenuItem)
#' }
DatabaseMenuItem <- R6::R6Class(
  classname = "DatabaseMenuItem",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field dbTable (`Table`) Module.
    dbTable = function() return(private$.dbTable),

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

    #' @param tableTitle (`character(1)`) Title to use for the cohort table.
    tableTitle = function(tableTitle) {
      if (missing(tableTitle)) {
        return(private$.tableTitle)
      } else {
        checkmate::assertCharacter(subItemText, len = 1)
        private$.tableTitle <- tableTitle
      }
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to display in the table, usually a `data.frame`-like object.
    #' @param text (`character(1)`) Menu item text.
    #' @param subItemText (`character(1)`) Sub menu item text.
    #' @param tableTitle (`character(1)`) Title to use for the cohort table.
    #'
    #' @return `self`
    initialize = function(
      data,
      text = "Databases",
      subItemText = "Database details",
      tableTitle = "Study databases"
    ) {
      super$initialize()
      private$.text <- text
      private$.subItemText <- subItemText
      private$.tableTitle <- tableTitle

      private$.dbTable <- DarwinShinyModules::Table$new(
        data = data,
        options = list(scrollX = TRUE, dom = 't'),
        filter = 'none'
      )

      private$.dbTable$parentNamespace <- self$namespace
      return(invisible(self))
    },

    #' @description validate
    #'
    #' @return `invisible(self)`
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
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
        tabName = "databases",
        shinydashboard::menuSubItem(
          text = private$.subItemText,
          tabName = "db_details"
        )
      )
    },

    #' tabItem
    #'
    #' @return `tabItem`
    tabItem = function() {
      tabItem(
        tabName = "db_details",
        private$.dbTable$UI()
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
      shiny::moduleServer(id = private$.moduleId, module = function(input, output, session) {
        promises::future_promise(private$.dbTable$server(input, output, session))
        print(self$dbTable$namespace)
      })
    }
  ),

  # Private ----
  private = list(
    .dbTable = NULL,
    .text = "",
    .subItemText = "",
    .tableTitle = ""
  )
)
