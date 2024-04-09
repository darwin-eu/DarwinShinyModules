#' @title Module
#'
#' @description
#' Module super class. This class is an `interface` and is not meant to be
#' directly used, but to be inherited.
#'
#' @field appId (`character(1)`) appId used for namespacing.
#' @field moduleName (`character(1)`) Name of the module.
#' @field instanceId (`character(1)`) Random ID of 10 capitalized letters.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' MyModule <- R6::R6Class(
#'   classname = "MyModule",
#'   inherit = Module,
#'
#'   public = list(
#'     UI = function() {
#'       shiny::tagList(
#'         shiny::tableOutput(outputId = shiny::NS(private$.appId, private$id("myTable"))),
#'         shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("moduleData")))
#'       )
#'     },
#'
#'     server = function(input, output, session) {
#'       output[[private$id("myTable")]] <- shiny::renderTable({
#'         head(private$.data)
#'       })
#'
#'       output[[private$id("moduleData")]] <- shiny::renderUI({
#'         shiny::HTML(paste(
#'           sprintf("App ID: %s", private$.appId),
#'           sprintf("Module Name: %s", private$.moduleName),
#'           sprintf("Instance ID: %s", private$.instanceId),
#'           sprintf("Namespace ID myTable: %s", private$id("myTable")),
#'           sprintf("Namespace ID moduleData: %s", private$id("moduleData")),
#'           sep = "<br/>"
#'         ))
#'       })
#'     }
#'   ),
#'
#'   private = list(
#'     .data = iris
#'   ),
#'
#'   active = list(
#'     data = function() return(private$.data)
#'   )
#' )
#'
#' myModule <- MyModule$new(appId = "app")
#'
#' ui <- shiny::fluidPage(
#'   myModule$UI()
#' )
#'
#' server <- function(input, output, session) {
#'   shiny::moduleServer(id = "app", module = function(input, output, session) {
#'     myModule$server(input, output, session)
#'   })
#' }
#'
#' if (interactive()) {
#'   shiny::shinyApp(ui, server)
#' }
Module <- R6::R6Class(
  classname = "Module",

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param appId (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(appId) {
      private$.appId <- appId
      private$.moduleName <- class(self)[1]
      private$.instanceId <- paste0(sample(x = LETTERS, size = 10), collapse = "")
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return (`invisible(self)`)
    validate = function() {},

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return (`tagList`)
    UI = function() {},

    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #'
    #' @return (`NULL`)
    server = function(input, output, session) {}
  ),

  # Private ----
  private = list(
    ## Fields ----
    .appId = "",
    .moduleName = "",
    .instanceId = "",

    ## Methods ----
    finalize = function() {},

    id = function(id) {
      paste(private$.moduleName, private$.instanceId, id, sep = "_")
    }
  ),

  # Active ----
  active = list(
    appId = function() return(private$.appId),
    moduleName = function() return(private$.moduleName),
    instanceId = function() return(private$.instanceId)
  )
)
