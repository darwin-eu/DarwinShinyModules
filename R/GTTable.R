#' @title GTTable Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' GTTable module that displays tables using `gt` that are supported by
#' `gt::render_gt()` and `gt::gt_output()`.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' gtTable <- GTTable$new(
#'   fun = gt::gt,
#'   args = list(data = iris)
#' )
#'
#' if(interactive()) {
#'   preview(gtTable)
#' }
GTTable <- R6::R6Class(
  classname = "GTTable",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field fun (`function`) Function to produce a `gt` table with, i.e `gt::gt`.
    fun = function() {
      return(private$.fun)
    },

    #' @field args (`list`) Arguments for said function as a named list i.e. `list(data = iris)`.
    args = function() {
      return(private$.args)
    }
  ),

  # Public ----
  public = list(

    #' @description
    #' Initializer method.
    #'
    #' @param fun (`function`) Function to produce a `gt` table with, i.e `gt::gt`.
    #' @param args (`list()`) Arguments for said function as a named list i.e. `list(data = iris)`.
    #'
    #' @returns `self`
    initialize = function(fun, args) {
      super$initialize()
      private$assertGtInstall()
      private$.fun <- fun
      private$.args <- args
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .fun = NULL,
    .args = NULL,

    .UI = function() {
      shiny::tagList(
        gt::gt_output(outputId = shiny::NS(private$.namespace, "gtTable"))
      )
    },

    .server = function(input, output, session) {
      output$gtTable <- gt::render_gt({
        do.call(private$.fun, private$.args)
      })
    },

    assertGtInstall = function() {
      if (!require("gt", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)) {
        stop("Required package: `gt` is not installed")
      }
    }
  )
)
