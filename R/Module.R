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
