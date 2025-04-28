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
  inherit = DarwinShinyModules::ShinyModule,

  # Public ----
  public = list(

    ## Methods ----
    #' @description initialize
    #'
    #' @param data Data to show in the picker, a vector of values
    #' @param label Picker label
    #' @param selected which items are selected by default
    #' @param multiple if multiple items are supported
    #'
    #' @return `self`
    initialize = function(data, label, selected = data, multiple = TRUE) {
      super$initialize()
      private$.data <- data
      private$.label <- label
      private$.selected <- selected
      private$.multiple <- multiple
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .data = NULL,
    .label = NULL,
    .selected = NULL,
    .multiple = TRUE,

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
    }
  )
)
