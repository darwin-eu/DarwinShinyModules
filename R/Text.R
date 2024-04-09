#' @title Text
#'
#' @include Module.R
#'
#' @description
#' Text Module
#'
#' @field markdown (`character(n)`) Lines read from the markdown-file supplied.
#'
#' @export
Text <- R6::R6Class(
  classname = "Text",
  inherit = Module,

  # PUblic ----
  public = list(
    ## Methods ----

    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param markdown (`character(n)`) Markdown.
    #'
    #' @return `self`
    initialize = function(appId, markdown) {
      super$initialize(appId)
      private$.markdown <- markdown
      return(invisible(self))
    },

    #' @description UI
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
    .markdown = ""
  ),

  # Active ----
  active = list(
    markdown = function() return(private$.markdown)
  )
)
