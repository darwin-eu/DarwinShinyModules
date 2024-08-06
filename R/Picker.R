#' @examples
#' library(DarwinShinyModules)
#'
#' picker <- Picker$new(appId = "app", data = mtcars$mpg, label = "MPG"))
#'
#' if (interactive()) {
#'   preview(picker)
#' }
Picker <- R6::R6Class(
  classname = "Picker",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to show in the picker, a vector of values
    #' @param label Picker label
    #' @param selected which items are selected by default
    #' @param multiple if multiple items are supported
    #'
    #' @return `self`
    initialize = function(appId, data, label, selected = data, multiple = TRUE) {
      super$initialize(appId)
      private$.data <- data
      private$.label <- label
      private$.selected <- selected
      private$.multiple <- multiple
      return(invisible(self))
    },

    #' @description UI
    #'
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          shinyWidgets::pickerInput(
            inputId = private$.appId,
            label = private$.label,
            choices = unique(private$.data),
            selected = unique(private$.selected),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = private$.multiple
          )
        )
      )
    },

    #' @description server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .label = NULL,
    .selected = NULL,
    .multiple = TRUE
  )
)
