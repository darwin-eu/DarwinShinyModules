CohortCodelist <- R6::R6Class(
  classname = "CohortCodelist",
  inherit = ShinyModule,

  # Active ----
  active = list(
    result = function(result) {
      if (missing(result)) {
        return(qs2::qs_deserialize(private$.result))
      } else {
        private$.checkResult(result)
        private$.result <- qs2::qs_serialize(result)
      }
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)

      self$result <- result
      private$.cdmNames <- getCDMNames(result)
      private$.cohortNames <- getCohortNames(result)

      private$.table <- Flextable$new(
        fun = visOmopResults::visOmopTable,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
    }
  ),

  # private ----
  private = list(
    .result = NULL,

    .table = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .UI = function() {
      shiny::fluidPage(
        private$.generalSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            private$.table$UI()
          )
        )
      )
    },

    .generalSettings = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmNames"),
            label = "CDM Name",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cohortNames"),
            label = "Target Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        mirai::mirai({
          result |>
            qs2::qs_deserialize()
        }, result = private$.result)
      })

      shiny::observe({
        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.table$server(input, output, session)
          }
        )
      })
    },

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assertClass(result, "summarised_result", add = collection)
      checkmate::assertTRUE(attr(result, "settings")$result_type == "summarise_dose_coverage", add = collection)
      checkmate::reportAssertions(collection)
    }
  )
)
