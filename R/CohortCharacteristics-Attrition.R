Attrition <- R6::R6Class(
  classname = "Attrition",
  inherit = DarwinShinyModules::ShinyModule,

  active = list(
    data = function () {
      return(private$.data)
    },

    widget = function() {
      return(private$.widget)
    }
  ),

  public = list(
    initialize = function(attrition) {
      super$initialize()
      private$.data <- attrition
      private$.widget <- DarwinShinyModules::PlotWidget$new(
        fun = CohortCharacteristics::plotCohortAttrition,
        args = list(result = attrition),
        title = NULL
      )
      private$.table <- DarwinShinyModules::Table$new(data = private$.data, title = NULL, filter = "none")
      private$.widget$parentNamespace <- self$namespace
      private$.widget$async <- TRUE
      private$.table$parentNamespace <- self$namespace
    }
  ),

  private = list(
    .data = NULL,
    .widget = NULL,
    .table = NULL,

    .UI = function() {
      shiny::tagList(
        if (!is.null(private$.data$cohort_name)) {
          shiny::selectInput(
            inputId = shiny::NS(self$namespace, "cohort"),
            label = "Cohort",
            choices = unique(private$.data$cohort_name)
          )
        },
        private$.widget$UI(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.widget$server(input, output, session)
      private$.table$server(input, output, session)

      shiny::observeEvent(input$cohort, {
        if (!is.null(private$.data$cohort_name)) {
          dataSubset <- private$.data %>%
            dplyr::filter(.data$cohort_name == input$cohort)

          private$.widget$args$result <- dataSubset
          private$.table$data <- dataSubset
        }
      })
    }
  )
)
