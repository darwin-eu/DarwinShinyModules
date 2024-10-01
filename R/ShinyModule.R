#' @title Module Super Class
#'
#' @description
#' This class is an `decorator` and is not meant to be directly used, but to be
#' inherited by other Modules.
#'
#' @template param_appId
#'
#' @export
ShinyModule <- R6::R6Class(
  classname = "ShinyModule",

  # Active ----
  active = list(
    #' @field instanceId (`character(1)`) Random ID of 10 capitalized letters.
    instanceId = function(instanceId) {
      if (missing(instanceId)) {
        return(private$.instanceId)
      } else {
        checkmate::assertCharacter(x = instanceId, len = 1)
        private$.instanceId <- instanceId
        return(invisible(self))
      }
    },

    #' @field parentNamespace (`character(1)`) Namespace of the parent module.
    parentNamespace = function(parentNamespace) {
      if (missing(parentNamespace)) {
        return(private$.parentNamespace)
      } else {
        checkmate::assertCharacter(x = parentNamespace, len = 1)
        private$.parentNamespace <- parentNamespace
        private$.namespace <- paste(c(private$.parentNamespace, private$.moduleId), collapse = "-")
        return(invisible(self))
      }
    },

    #' @field moduleName (`character(1)`) Name of the module.
    moduleName = function() {
      return(private$.moduleName)
    },

    moduleId = function() {
      return(private$.moduleId)
    },

    namespace = function() {
      return(private$.namespace)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return
    #' (`self`)
    initialize = function() {
      private$.moduleName <- class(self)[1]
      private$.instanceId <- private$makeInstanceId()
      private$.moduleId <- sprintf("%s-%s", private$.moduleName, private$.instanceId)
      private$.namespace <- c(private$.parentNamespace, private$.moduleId)
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "instanceId",
        x = private$.instanceId,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "parentNamespace",
        x = private$.parentNamespace,
        len = 1,
        null.ok = TRUE,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleName",
        x = private$.moduleName,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleId",
        x = private$.moduleId,
        len = 1,
        add = assertions
      )

      checkmate::reportAssertions(assertions)
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
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .moduleName = "",
    .instanceId = "",
    .moduleId = "",
    .parentNamespace = NULL,
    .namespace = "",


    ## Methods ----
    finalize = function() {
      return(NULL)
    },

    makeInstanceId = function(n = 20) {
      items <- c(letters, LETTERS, c(1:9), c("_"))
      paste0(sample(x = items, size = n), collapse = "")
    }
  )
)
