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
#' DatabaseMenuItem <- DatabaseMenuItem$new(appId = "id", data = mtcars)
#'
#' if (interactive()) {
#'   preview(DatabaseMenuItem)
#' }
DatabaseMenuItem <- R6::R6Class(
  classname = "DatabaseMenuItem",
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
      private$.dbTable <- Table$new(appId = appId,
                                    data = data,
                                    options = list(scrollX = TRUE, dom = 't'),
                                    filter = 'none')

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
    UI = function(text = "Databases",
                  subItemText = "Database details") {
      menuItem(
        text = text,
        tabName = "databases",
        menuSubItem(
          text = subItemText,
          tabName = "db_details"
        )
      )
    },

    #' tabItem
    #'
    #' @param tableTitle (`character(1)`) Title to use for the cohort table.
    #'
    #' @return `tabItem`
    tabItem = function(tableTitle = "Study databases") {
      tabItem(
        tabName = "db_details",
        private$.dbTable$UI(tableTitle)
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
      promises::future_promise(private$.dbTable$server(input, output, session))
    }
  ),

  # Active ----
  active = list(
    #' @field dbTable (`Table`) Module.
    dbTable = function() return(private$.dbTable)
  ),

  # Private ----
  private = list(
    .data = NULL,
    .dbTable = NULL
  )
)
