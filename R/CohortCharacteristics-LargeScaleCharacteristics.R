# Copyright 2026 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title LargeScaleCharacteristics Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' LargeScaleCharacteristics module that displays tables and plots of the
#' `summarised_result` object created by `CohortCharacteristics::summariseLargeScaleCharacteristics()`.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   CDMConnector::requireEunomia()
#'    con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
#'    cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")
#'
#'    cdm <- DrugUtilisation::generateIngredientCohortSet(
#'      cdm = cdm,
#'      name = "my_cohort",
#'      ingredient = c("warfarin", "acetaminophen")
#'   )
#'
#'    cdm$my_cohort <- cdm$my_cohort |>
#'      PatientProfiles::addAge(ageGroup = list(
#'       `0 to 17` = c(0, 17),
#'       `>=18` = c(18, Inf)
#'     )) |>
#'     PatientProfiles::addSex()
#'
#'   result <- CohortCharacteristics::summariseLargeScaleCharacteristics(
#'     cohort = cdm$my_cohort,
#'     eventInWindow = "condition_occurrence",
#'     strata = list("age_group", "sex")
#'   )
#'
#'   lscMod <- LargeScaleCharacteristics$new(result = result)
#'   DarwinShinyModules::preview(lscMod)
#' }
LargeScaleCharacteristics <- R6::R6Class(
  classname = "LargeScaleCharacteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field result (`summarised_result`)
    result = function(result) {
      if (missing(result)) {
        return(private$.result)
      } else {
        checkmate::assertClass(result, "summarised_result")
        checkmate::assertSubset(
          x = omopgenerics::settings(result)$result_type,
          choices = c("summarise_large_scale_characteristics"),
          .var.name = "result_type"
        )
        private$.result <- result
      }
    },

    #' @field cdmNames (`character(n)`)
    cdmNames = function() {
      return(private$.cdmNames)
    },

    #' @field cohortNames (`character(n)`)
    cohortNames = function() {
      return(private$.cohortNames)
    },

    #' @field strata (`character(n)`)
    strata = function() {
      return(private$.strata)
    },

    #' @field windows (`character(n)`)
    windows = function() {
      return(private$.windows)
    },

    #' @field table (`ShinyModule`)
    table = function() {
      return(private$.table)
    },

    #' @field tableTop (`ShinyModule`)
    tableTop = function() {
      return(private$.tableTop)
    },

    #' @field plot (`ShinyModule`)
    plot = function() {
      return(private$.plot)
    },

    #' @field plotCompared (`ShinyModule`)
    plotCompared = function() {
      return(private$.plotCompared)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method.
    #'
    #' @param result (`summarised_result`) Object created by `CohortCharacteristics::summariseLargeScaleCharacteristics()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result, ...) {
      private$assertInstall("CohortCharacteristics", "1.0.0")
      super$initialize(...)
      if ("summarised_result" %in% class(result)) {
        private$.result <- result
      } else {
        stop("Data has to be of class: `summarised_result`")
      }

      private$.initTable()
      private$.initTableTop()
      private$.initPlot()
      private$.initPlotCompared()

      private$.setFilterValues()

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,

    # Nested modules
    .table = NULL,
    .tableTop = NULL,
    .plot = NULL,
    .plotCompared = NULL,

    # Filter values
    .cdmNames = NULL,
    .cohortNames = NULL,
    .strata = NULL,
    .windows = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    ## UI ----
    .UI = function() {
      shiny::fluidPage(
        private$.uiGeneralFilters(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Large Scale Characteristics",
            private$.tableUI()
          ),
          shiny::tabPanel(
            title = "Top Concepts",
            private$.tableTopUI()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.plotUI()
          ),
          shiny::tabPanel(
            title = "Compare Plot",
            private$.plotComparedUI()
          )
        )
      )
    },

    .uiGeneralFilters = function() {
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
        )
      )
    },

    .tableUI = function() {
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCompareBy"),
            label = "Compare By",
            choices = c(
              "cdm_name", "cohort_name", "variable_level", "type",
              private$.result |>
                dplyr::pull(.data$strata_name) |>
                unique()
            ),
            selected = "cdm_name"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableSMDReference"),
            label = "SMD reference",
            choices = c()
          )
        ),
        shiny::column(width = 9, private$.table$UI())
      )
    },

    .tableTopUI = function() {
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopStrata"),
            label = "Strata",
            choices = private$.strata,
            selected = private$.strata[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopVariableLevel"),
            label = "Window",
            choices = private$.windows,
            selected = private$.windows[1],
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopN"),
            label = "Top Concepts",
            choices = c(5, 10, 25, 50, 100),
            selected = 5
          )
        ),
        shiny::column(
          width = 10,
          private$.tableTop$UI()
        )
      )
    },

    .plotUI = function() {
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetX"),
            label = "Horizontal Facet",
            choices = availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacetY"),
            label = "Vertical Facet",
            choices = availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotColour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(width = 10, private$.plot$UI())
      )
    },

    .plotComparedUI = function() {
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedColour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = "cdm_name",
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedReference"),
            label = "Reference",
            choices = private$.cdmNames,
            selected = private$.cdmNames[1],
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedFacetX"),
            label = "Horizontal Facet",
            choices = availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedFacetY"),
            label = "Vertical Facet",
            choices = availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 9,
          private$.plotCompared$UI()
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.updateTable(input, output, session)
      private$.updatePlotCompared(input, output, session)

      private$.serverTable(input, output, session)
      private$.serverTopTable(input, output, session)
      private$.serverPlot(input, output, session)
      private$.serverComparePlot(input, output, session)
    },

    .updateTable = function(input, output, session) {
      shiny::observeEvent(list(input$tableCompareBy, input$cdmName, input$cohortName), {
        choices <- if (input$tableCompareBy == "cdm_name") {
          cdmNames <- private$.result |>
            dplyr::distinct(.data$cdm_name) |>
            dplyr::pull(.data$cdm_name)
          cdmNames[cdmNames %in% input$cdmName]
        } else if (input$tableCompareBy == "cohort_name") {
          cohortNames <- private$.result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::distinct(.data$group_level) |>
            dplyr::pull(.data$group_level)
          cohortNames[cohortNames %in% input$cohortName]
        } else if (input$tableCompareBy == "variable_level") {
          private$.result |>
            dplyr::distinct(.data$variable_level) |>
            dplyr::pull(.data$variable_level)
        } else if (input$tableCompareBy == "type") {
          settings <- omopgenerics::settings(private$.result)
          settings$type |>
            unique()
        } else if (input$tableCompareBy %in% private$.strata) {
          private$.result |>
            dplyr::filter(.data$strata_name == input$tableCompareBy) |>
            dplyr::distinct(.data$strata_level) |>
            dplyr::pull(.data$strata_level)
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "tableSMDReference",
          choices = choices
        )
      })
    },

    .updatePlotCompared = function(input, output, session) {
      shiny::observeEvent(input$plotComparedColour, {
        choices <- if (input$plotComparedColour == "cdm_name") {
          unique(private$.cdm_names)
        } else if (input$plotComparedColour == "cohort_name") {
          private$.result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::pull(.data$group_level) |>
            unique()
        } else if (input$plotComparedColour == "variable_level") {
          private$.result |>
            dplyr::pull(.data$variable_level) |>
            unique()
        } else if (input$plotComparedColour == "type") {
          settings <- omopgenerics::settings(private$.result)
          settings$type |>
            unique()
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "plotComparedReference",
          choices = choices
        )
      })
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$tableCompareBy,
        input$tableSMDReference,
        input$cdmName,
        input$cohortName
      ), {
        private$.table$args$compareBy <- input$tableCompareBy
        private$.table$args$smdReference <- input$tableSMDReference

        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$group_level %in% input$cohortName
          )

        private$.table$server(input, output, session)
      })
    },

    .serverTopTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$tableTopN,
        input$cdmName,
        input$cohortName,
        input$tableTopStrata,
        input$tableTopVariableLevel
      ), {
        private$.tableTop$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$cdmName,
            .data$strata_name %in% input$tableTopStrata,
            .data$variable_level %in% input$tableTopVariableLevel
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.tableTop$args$topConcepts <- input$tableTopN
        private$.tableTop$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName,
        input$cohortName,
        input$plotFacetX,
        input$plotFacetY,
        input$plotColour
      ), {
        private$.plot$args$facet <- makeFacetFormula(
          facetX = input$plotFacetX,
          facetY = input$plotFacetY
        )

        facets <- unique(c(input$plotFacetX, input$plotFacetY))

        result <- if (is.null(facets)) {
          private$.result |>
            dplyr::filter(.data$strata_name == "overall")
        } else {
          private$.result |>
            dplyr::filter(.data$strata_name %in% c("overall", facets))
        }

        private$.plot$args$result <- result |>
          dplyr::filter(.data$cdm_name %in% input$cdmName) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$cohortName)

        private$.plot$args$colour <- input$plotColour
        private$.plot$server(input, output, session)
      })
    },

    .serverComparePlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdmName, input$cohortName,
        input$plotComparedColour,
        input$plotComparedReference,
        input$plotComparedFacetX,
        input$plotComparedFacetY
      ), {
        private$.plotCompared$args$colour <- input$plotComparedColour
        private$.plotCompared$args$reference <- input$plotComparedReference

        private$.plotCompared$args$facet <- makeFacetFormula(
          facetX = input$plotComparedFacetX,
          facetY = input$plotComparedFacetY
        )

        facets <- unique(c(input$plotComparedFacetX, input$plotComparedFacetY))

        result <- if (is.null(facets)) {
          private$.result |>
            dplyr::filter(.data$strata_name == "overall")
        } else {
          private$.result |>
            dplyr::filter(.data$strata_name %in% c("overall", facets))
        }

        private$.plotCompared$args$result <- result |>
          dplyr::filter(.data$cdm_name %in% input$cdmName) |>
          omopgenerics::filterGroup(
            .data$cohort_name %in% input$cohortName
          )

        private$.plotCompared$server(input, output, session)
      })
    },

    ## Init ----
    .initTable = function() {
      private$.table <- DarwinShinyModules::DTTable$new(
        fun = CohortCharacteristics::tableLargeScaleCharacteristics,
        args = list(result = private$.result, type = "DT"),
        height = "80vh",
        parentNamespace = self$namespace
      )
    },

    .initTableTop = function() {
      args <- if (utils::packageVersion("CohortCharacteristics") > package_version("1.0.0")) {
        list(result = private$.result, type = "flextable", style = "darwin")
      } else {
        list(result = private$.result, type = "flextable")
      }
      private$.tableTop <- DarwinShinyModules::Flextable$new(
        fun = CohortCharacteristics::tableTopLargeScaleCharacteristics,
        args = args,
        heigth = "80vh",
        parentNamespace = self$namespace
      )
    },

    .initPlot = function() {
      args <- if (utils::packageVersion("CohortCharacteristics") > package_version("1.0.0")) {
        list(result = private$.result, style = "darwin")
      } else {
        list(result = private$.result)
      }
      private$.plot <- DarwinShinyModules::PlotPlotly$new(
        fun = CohortCharacteristics::plotLargeScaleCharacteristics,
        args = args,
        title = NULL,
        height = "80vh",
        parentNamespace = self$namespace
      )
    },

    .initPlotCompared = function() {
      args <- if (utils::packageVersion("CohortCharacteristics") > package_version("1.0.0")) {
        list(result = private$.result, style = "darwin")
      } else {
        list(result = private$.result)
      }

      plotFun <- function(...) {
        CohortCharacteristics::plotComparedLargeScaleCharacteristics(...) +
          ggplot2::coord_fixed(ratio = 1 / 1)
      }

      private$.plotCompared <- DarwinShinyModules::PlotPlotly$new(
        fun = plotFun,
        args = args,
        width = "108vh",
        height = "80vh",
        inline = TRUE,
        title = NULL,
        parentNamespace = self$namespace
      )
    },

    # Helpers ----
    .setFilterValues = function() {
      private$.cdmNames <- getCDMNames(private$.result)
      private$.cohortNames <- getCohortNames(private$.result)
      private$.strata <- getStrata(private$.result)

      private$.windows <- private$.result |>
        dplyr::distinct(.data$variable_level) |>
        dplyr::pull(.data$variable_level)
    }
  )
)

# Functions ----
#' moduleLargeScaleCharacteristics
#'
#' @param result (`summarised_result`) Result from the `summariseLargeScaleCharacteristics()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   moduleLargeScaleCharacteristics(result)
#' }
moduleLargeScaleCharacteristics <- function(result, .softValidation) {
  assertType(result, type = "summarise_large_scale_characteristics")
  checkCDMNames(result, .softValidation)
  LargeScaleCharacteristics$new(result)
}

#' shinyLargeScaleCharacteristics
#'
#' @param result (`summarised_result`) Result from the `summariseLargeScaleCharacteristics()` function from the `CohortCharacteristics` pacakge.
#' @param .softValidation (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a warning.
#'
#' @returns `shiny.appobj`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinyLargeScaleCharacteristics(result)
#' }
shinyLargeScaleCharacteristics <- function(result, .softValidation) {
  launchBslibApp(
    list(
      LargeScaleCharacteristics = moduleLargeScaleCharacteristics(result, .softValidation)
    )
  )
}
