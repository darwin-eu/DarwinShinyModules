DoseCoverage <- R6::R6Class(
  classname = "DoseCoverage",
  inherit = ShinyModule,

  # Active ----
  active = list(
    result = function(result) {
      if (missing(result)) {
        return(qs2::qs_deserialize(private$.result))
      } else {
        private$.validate(result)
        private$.result <- qs2::qs_serialize(result)
      }
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)

      private$.validate(result)

      private$.cdmNames <- getCDMNames(result)
      private$.cohortNames <- getCohortNames(result)
      private$.strataCols <- DrugUtilisation::strataColumns(result)
      private$.strataGroups <- result |>
        dplyr::distinct(.data$strata_name) |>
        dplyr::pull()
      private$.tableCols <- availableTableColumns(result)
      private$.result <- qs2::qs_serialize(result)

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableDoseCoverage,
        args = list(type = "flextable", style = "darwin")
      )
    }
  ),

  # Private ----
  private = list(
    .result = NULL,

    .tableCols = NULL,
    .cdmNames = NULL,
    .cohortNames = NULL,
    .strataCols = NULL,
    .strataGroups = NULL,

    .table = NULL,

    .validate = function() {},

    .UI = function() {
      shiny::fluidPage(
        private$.uiGeneralSettings(),
        shiny::column(
          width = 2,
          private$.uiTableSettings()
        ),
        shiny::column(
          width = 10,
          private$.table$UI()
        )
      )
    },

    .uiGeneralSettings = function() {
      shiny::tagList(
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdmName"),
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
            inputId = shiny::NS(self$namespace, "cohortName"),
            label = "Cohort Name",
            choices = private$.cohortNames,
            selected = private$.cohortNames[1],
            multiple = TRUE,
            options = private$.pickerOptions
          )
        ),
        shiny::div(
          style = "display: inline-block;",
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "strata"),
            label = "Strata",
            choices = private$.strataGroups,
            selected = "overall",
            multiple = TRUE,
            options = private$.pickerOptions
          )
        )
      )
    },

    .uiTableSettings = function() {
      shiny::fluidRow(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "header"),
          label = "Header",
          choices = private$.tableCols,
          selected = c("variable_name", "estimate_name"),
          multiple = TRUE,
          options = private$.pickerOptions
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "groupColumn"),
          label = "Group Column",
          choices = private$.tableCols,
          selected = c("cdm_name", "ingredient_name"),
          multiple = TRUE,
          options = private$.pickerOptions
        )
      )
    },

    .server = function() {
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
    }
  )
)
