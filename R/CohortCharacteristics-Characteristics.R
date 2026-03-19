# Copyright 2024 DARWIN EU®
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

#' @title Characterisitcs Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Characteristics module that shows characteristics results (table and plot) from the CohortCharacteristics package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "CohortCharacteristics",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     result <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/CohortCharacteristics/1.1.1/characteristics.csv"
#'     ))
#'
#'     charMod <- Characteristics$new(result = result)
#'
#'     ui <- shiny::fluidPage(
#'       charMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       charMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
Characteristics <- R6::R6Class(
  classname = "Characteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param result (`summarised_result`) Result of `CohortCharacteristics::summariseCharacteristics()`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(result = NULL, ...) {
      private$assertInstall("CohortCharacteristics", "1.0.0")
      super$initialize(...)
      private$.result <- result

      private$.table <- DarwinShinyModules::Flextable$new(
        fun = CohortCharacteristics::tableCharacteristics,
        args = list(
          result = private$.result,
          type = "flextable",
          style = "darwin"
        ),
        parentNamespace = self$namespace
      )

      private$.plot <- DarwinShinyModules::PlotStatic$new(
        fun = CohortCharacteristics::plotCharacteristics,
        args = list(style = "darwin"),
        parentNamespace = self$namespace
      )
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .result = NULL,
    .table = NULL,
    .plot = NULL,

    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          # Table ----
          shiny::tabPanel(
            title = "Table",
            private$.tableUI()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.plotUI()
          ),
          shiny::tabPanel(
            title = "Info",
            private$.infoUI()
          )
        )
      )
    },

    .tableUI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "header"),
            label = "Headers",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            selected = c("cdm_name", "cohort_name"),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Columns",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "hide"),
            label = "Columns to hide",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            selected = c(CohortCharacteristics::additionalColumns(private$.result), CohortCharacteristics::settingsColumns(private$.result)),
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 10,
          private$.table$UI()
        )
      )
    },

    .plotUI = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "plotType"),
          label = "Plot Type",
          choices = c("barplot", "scatterplot", "boxplot"),
          selected = "boxplot"
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "facet"),
          label = "Facet",
          choices = CohortCharacteristics::availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "colour"),
          label = "Colour",
          choices = CohortCharacteristics::availablePlotColumns(private$.result),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "estimate_name"),
          label = "Estimate",
          choices = unique(private$.result$estimate_name),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "variable_name"),
          label = "Variable",
          choices = unique(private$.result$variable_name),
          selected = unique(private$.result$variable_name)[1]
        ),
        private$.plot$UI()
      )
    },

    .infoUI = function() {
      shiny::markdown("
      ## Characteristics
      The characteristics module contains two sections (`Table` and `Plot`).

      User input in the module reflect the parameters in the[`tableCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/tableCharacteristics.html) and [`plotCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/plotCharacteristics.html) functions from the [`CohortCharacteristics`](https://darwin-eu.github.io/CohortCharacteristics/index.html) package.
      ")
    },

    .server = function(input, output, session) {
      output$foo <- shiny::renderText({
        input$foo
      })
      private$.tableServer(input, output, session)
      private$.plotServer(input, output, session)
    },

    .tableServer = function(input, output, session) {
      shiny::observeEvent(list(input$header, input$groupColumn, input$hide), {
        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn
        private$.table$args$hide <- input$hide
        private$.table$server(input, output, session)
      })
    },

    .plotServer = function(input, output, session) {
      shiny::observeEvent(list(input$plotType, input$facet, input$colour, input$estimate_name, input$variable_name), {
        private$.plot$args$plotType <- input$plotType
        private$.plot$args$facet <- input$facet
        private$.plot$args$colour <- input$colour

        estimate_name <- if (is.null(input$estimate_name)) unique(private$.result$estimate_name)

        private$.plot$args$result <- private$.result |>
          dplyr::filter(.data$estimate_name %in% estimate_name) |>
          dplyr::filter(.data$variable_name %in% input$variable_name)

        private$.plot$server(input, output, session)
      })
    }
  ),

  # Active ----
  active = list(
    #' @field result (`summarised_result`) Result of `CohortCharacteristics::summariseCharacteristics()`.
    result = function(result) {
      if (missing(result)) {
        return(private$.result)
      } else {
        collection <- checkmate::makeAssertCollection()
        checkmate::assertClass(x = result, classes = c("summarised_result", "omop_result", "tbl_df", "tbl", "data.frame"), add = collection)
        try({
          settings <- omopgenerics::settings(result)
          checkmate::assertTRUE(settings$result_type == "summarise_characteristics", add = collection)
          checkmate::assertTRUE(settings$package_name == "CohortCharacteristics", add = collection)
        }, silent = TRUE)
        checkmate::reportAssertions(collection)
        private$.result <- result
      }
    }
  )
)
