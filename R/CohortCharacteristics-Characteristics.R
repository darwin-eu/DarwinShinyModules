Characteristics <- R6::R6Class(
  classname = "Characteristics",
  inherit = DarwinShinyModules::ShinyModule,

  active = list(
    result = function() {
      return(.private$result)
    },

    raw = function() {
      return(.private$raw)
    },

    tidy = function() {
      return(.private$tidy)
    }
  ),

  public = list(
    initialize = function(result = NULL) {
      super$initialize()
      private$.result <- result

      private$.raw <- DarwinShinyModules::Table$new(
        data = private$.result,
        title = NULL
      )
      private$.raw$parentNamespace <- self$namespace

      private$.tidy <- DarwinShinyModules::GTTable$new(
        fun = CohortCharacteristics::tableCharacteristics,
        args = list(
          result = private$.result
        )
      )
      private$.tidy$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .result = NULL,
    .raw = NULL,
    .tidy = NULL,

    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "tidy",
            private$.tidy$UI()
          ),
          shiny::tabPanel(
            title = "raw",
            private$.raw$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      private$.raw$server(input, output, session)
      private$.tidy$server(input, output, session)
    }
  )
)
