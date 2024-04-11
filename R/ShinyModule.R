#' @title Module Super Class
#'
#' @description
#' This class is an `interface` and is not meant to be directly used, but to be
#' inherited by other Modules.
#'
#' @template param_appId
#'
#' @export
ShinyModule <- R6::R6Class(
  classname = "ShinyModule",

  # Public ----
  public = list(
    ## Fields ----
    #' @field instanceId (`character(1)`) Random ID of 10 capitalized letters.
    instanceId = "",
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return
    #' (`self`)
    initialize = function(appId) {
      private$.appId <- appId
      private$.moduleName <- class(self)[1]
      self$instanceId <- paste0(sample(x = LETTERS, size = 10), collapse = "")
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return
    #' (`tagList`)
    UI = function() {
      return(shiny::tagList())
    },

    #' @description
    #' Method to handle the back-end.
    #'
    #' @template param_input
    #' @template param_output
    #' @template param_session
    #'
    #' @return
    #' (`NULL`)
    server = function(input, output, session) {
      return(NULL)
    },

    #' @description
    #' Create an instance unique ID for referencing objects in the input and
    #' output environments.
    #'
    #' @param id (`character(1)`) ID used for `outputId` in output() functions
    #' and to reference in the input environment.
    id = function(id) {
      paste(private$.moduleName, self$instanceId, id, sep = "_")
    }
  ),

  # Active ----
  active = list(
    #' @field appId (`character(1)`) appId used for namespacing.
    appId = function(rhs) {
      return(private$.appId)
    },

    #' @field moduleName (`character(1)`) Name of the module.
    moduleName = function(rhs) {
      return(private$.moduleName)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .appId = "",
    .moduleName = "",

    ## Methods ----
    finalize = function() {
      return(NULL)
    }
  )
)
