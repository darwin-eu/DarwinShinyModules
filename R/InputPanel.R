#' @title InputPanel
#'
#' @include ShinyModule.R
#'
#' @template param_appId
#' @param funs (`list()`) Named list of xInput functions used `list(funA = shiny::selectInput)`.
#' @param args (`list()`) Named list of arguments used by xInput functions `list(funA = list(inputId = "name", label = "name"))`
#'
#' @description
#' InputPanel Module.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' inputPanel <- InputPanel$new(
#'   funs = list(
#'     select = shiny::selectInput,
#'     text = shiny::textInput
#'   ),
#'   args = list(
#'     select = list(inputId = "select", choices = c("a", "b"), label = "select"),
#'     text = list(inputId = "text", label = "text")
#'   )
#' )
#'
#' if (interactive()) {
#'   preview(inputPanel)
#' }
InputPanel <- R6::R6Class(
  classname = "InputPanel",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field parentNamespace (`character(1)`) Namespace of the parent module.
    parentNamespace = function(parentNamespace) {
      super$parentNamespace <- parentNamespace
      private$updateIds()
      return(invisible(self))
    },

    #' @field funs (`list()`) Named list of xInput functions used `list(funA = shiny::selectInput)`.
    funs = function() return(private$.funs),

    #' @field args (`list()`) Named list of arguments used by xInput functions `list(funA = list(inputId = "name", label = "name"))`.
    args = function() return(private$.args),

    #' @field inputValues (`reactiveValues`) Values passed from the input fields.
    inputValues = function() return(private$.inputValues)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return (`invisible(self)`)
    initialize = function(funs, args) {
      super$initialize()
      private$.funs <- funs
      private$.args <- args
      private$updateIds()
      self$validate()
      return(invisible(self))
    },

    #' @description
    #' Validation method
    #'
    #' @return (`self`)
    validate = function() {
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return (`tagList`)
    UI = function() {
      shiny::tagList(
        lapply(names(private$.funs), function(name) {
          # shiny::column(
          # width = 12L,
          do.call(what = private$.funs[[name]], args = private$.args[[name]])
          # )
        })
      )
    },

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
    server = function(input, output, session) {
      shiny::moduleServer(id = private$.moduleId, module = function(input, output, session) {
        private$.inputValues <- shiny::reactiveValues()

        lapply(names(private$.args), function(label) {
          shiny::observeEvent(input[[label]], {
            private$.inputValues[[label]] <- input[[label]]
          })
        })
      })
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .funs = NULL,
    .args = NULL,
    .inputValues = NULL,

    updateIds = function() {
      for (name in names(private$.args)) {
        if (!is.null(private$.args[[name]]$inputId)) {
          private$.args[[name]]$inputId <- shiny::NS(private$.namespace, name)
        }
      }
    }
  )
)
