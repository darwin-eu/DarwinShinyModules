#' @title Text
#'
#' @include Module.R
#'
#' @description
#' Text Module
#'
#' @field mdText (`character(n)`) Lines read from the markdown-file supplied.
#'
#' @export
Text <- R6::R6Class(
  classname = "Text",
  inherit = Module,

  # PUblic ----
  public = list(
    ## Methods ----

    #' initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param mdFile (`character(1)`) Path to the markdown-file.
    #'
    #' @return `self`
    initialize = function(appId, mdFile) {
      super$initialize(appId)
      private$.mdText <- readLines(mdFile)
      return(invisible(self))
    },

    #' UI
    #'
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::markdown(private$.mdText)
      )
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .mdText = ""
  ),

  # Active ----
  active = list(
    mdText = function() return(private$.mdText)
  )
)
