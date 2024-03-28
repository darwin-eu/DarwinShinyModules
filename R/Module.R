#' @title Module
#'
#' @description
#' Module super class
#'
#' @export
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
      self$validate()
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return (`invisible(self)`)
    validate = function() {
      private$assertDependencies()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(.var.name = "appId", x = private$.appId, len = 1)
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

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

    assertDependencies = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertTRUE(require("shiny", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("shinydashboard", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("ggplot2", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("plotly", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("dplyr", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::reportAssertions(assertions)
    },

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
