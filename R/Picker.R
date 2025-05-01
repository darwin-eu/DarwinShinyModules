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
    #' @param parentId parent input identifier
    #' @param data Data to show in the picker, a vector of values
    #' @param label Picker label
    #' @param selected which items are selected by default
    #' @param multiple if multiple items are supported
    #'
    #' @return `self`
    initialize = function(parentId, data, label, selected = data, multiple = TRUE) {
      super$initialize()
      private$.inputId <- id
      private$.namespace <- paste(c(parentId, private$.moduleId), collapse = "-")
      private$.picker <- shinyWidgets::pickerInput(
        inputId = private$.namespace,
        label = label,
        choices = unique(data),
        selected = unique(selected),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
        multiple = multiple
      )
      return(invisible(self))
    },

    selected = function() {
      return(unique(private$.selected))
    }
  ),

  # Private ----
  private = list(
    .inputId = NULL,
    .selected = NULL,
    .picker = NULL,
    .pickerId = NULL,

    ## Methods ----
    .server = function(input, output, session) {
      private$setBindings(input)
    },

    #' @description UI
    #'
    #' @return `shiny.tag.list`
    .UI = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          private$.picker
        )
      )
    },

    setBindings = function(input) {
      shiny::observeEvent(eventExpr = private$.namespace, {
        print(paste0("inside observeEvent:", private$.namespace))
        print(input[[private$.namespace]])
        private$.selected <- input[[private$.namespace]]
      })
    }
  )
)
