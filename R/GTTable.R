GTTable <- R6::R6Class(
  classname = "GTTable",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(),

  # Public ----
  public = list(
    initialize = function(fun, args) {
      super$initialize()
      private$assertGtInstall()
      private$.fun <- fun
      private$.args <- args
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
