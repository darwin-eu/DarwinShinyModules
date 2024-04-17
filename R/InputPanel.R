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
#'   appId = "id",
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

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return (`invisible(self)`)
    initialize = function(appId, funs, args) {
      super$initialize(appId)
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
        shiny::inputPanel(
          shinydashboard::box(
            width = "100%",
            lapply(names(private$.funs), function(name) {
              shiny::column(
                width = 12L / length(private$.funs),
                do.call(what = private$.funs[[name]], args = private$.args[[name]])
              )
            })
          )
        )
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
      shiny::observeEvent({
        unlist(lapply(names(private$.args), function(name) {
          input[[private$unNS(private$.args[[name]]$inputId)]]
        }))
      }, {
        unlist(lapply(names(private$.args), function(name) {
          idComponents <- unlist(strsplit(
            x = private$unNS(private$.args[[name]]$inputId),
            split = "_"
          ))
          baseId <- idComponents[length(idComponents)]
          private$.inputValues[[baseId]] <- input[[private$unNS(private$.args[[name]]$inputId)]]
        }))
      })
    }
  ),

  # Active ----
  active = list(
    #' @field funs (`list()`) Named list of xInput functions used `list(funA = shiny::selectInput)`.
    funs = function() return(private$.funs),

    #' @field args (`list()`) Named list of arguments used by xInput functions `list(funA = list(inputId = "name", label = "name"))`.
    args = function() return(private$.args),

    #' @field inputValues (`reactiveValues`) Values passed from the input fields.
    inputValues = function() return(private$.inputValues)
  ),

  # Private ----
  private = list(
    ## Fields ----
    .funs = NULL,
    .args = NULL,
    .inputValues = shiny::reactiveValues(),

    ## Methods ----
    finalize = function() {},

    updateIds = function() {
      for (name in names(private$.args)) {
        if (!is.null(private$.args[[name]]$inputId)) {
          private$.args[[name]]$inputId <- shiny::NS(private$.appId, self$id(private$.args[[name]]$inputId))
        }
      }
    },

    unNS = function(x) {
      strsplit(x = x, split = "-", fixed = 1)[[1]][[2]]
    }
  )
)
