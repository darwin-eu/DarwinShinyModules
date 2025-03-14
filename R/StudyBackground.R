#' @title StudyBackground
#'
#' @include ShinyModule.R
#'
#' @description
#' StudyBackground Module that contains background information and the EUPAS.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' studyBackground <- StudyBackground$new(
#'   background = "./background.md",
#'   EUPAS = "EUPAS9999999"
#' )
#'
#' if (interacitve()) {
#'   preview(studyBackground)
#' }
StudyBackground <- R6::R6Class(
  classname = "StudyBackground",
  inherit = ShinyModule,

  active = list(
    #' @field background (`character(n)`) Either the direct background, or the
    #' contents of a markdown (.md) file.
    background = function() {
      return(private$.background)
    },

    #' @field EUPAS (`character(1)`) EUPAS belonging to the study.
    EUPAS = function() {
      return(private$.EUPAS)
    },

    #' @field text (`Text`) A Text module.
    text = function() {
      return(private$.text)
    }
  ),

  public = list(
    #' @description
    #' initializer method
    #'
    #' @param background (`character(n)`) Either a direct background description
    #' or a file path pointing to a markdown (.md) file.
    #' @param EUPAS (`character(1)`) EUPAS belonging to the study.
    #'
    #' @returns `invisible(self)`
    initialize = function(background, EUPAS) {
      super$initialize()
      private$.background <- private$parseSection(background)
      private$.EUPAS <- EUPAS

      private$.text <- Text$new(
        markdown = c(
          sprintf("**%s**", private$.EUPAS),
          "\n\n",
          "## Background",
          private$.background
        )
      )
      private$.text$parentNamespace <- self$namespace
      return(invisible(self))
    }
  ),

  private = list(
    .background = "",
    .EUPAS = "",
    .text = NULL,

    .UI = function() {
      shiny::wellPanel(
        private$.text$UI()
      )
    },

    parseSection = function(section) {
      if (all(file.exists(section))) {
        readLines(section)
      } else {
        section
      }
    }
  )
)
