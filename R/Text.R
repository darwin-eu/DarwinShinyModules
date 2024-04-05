#' @title Text
#'
#' @description
#' Text Module
#'
#' @export
Text <- R6::R6Class(
  classname = "Text",
  inherit = Module,
  public = list(
    initialize = function(appId, mdFile) {
      super$initialize(appId)
      private$.mdText <- readLines(mdFile)
    },

    server = function() {},
    ui = function() {
      shiny::markdown(private$.mdText)
    }
  ),
  private = list(
    .mdText = ""
  ),
  active = list(
    mdText = function() return(private$.mdText)
  )
)
