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

ProportionOfPatientsCovered <- R6::R6Class(
  classname = "ProportionOfPatientsCovered",
  inherit = ShinyModule,

  active = list(
    result = function(result) {
      if (missing(result)) {
        return(qs2::qs_deserialize(private$.serialResult))
      } else {
        private$.checkResult(result)
        private$.serialResult <- qs2::qs_serialize(result)
      }
    },

    table = function(table) {
      return(private$.table)
    },

    plot = function(plot) {
      return(private$.plot)
    }
  ),

  public = list(
    initialize = function(result, ...) {
      private$.checkResult(result)

      private$.cdmNames <- getCDMNames(result)
      private$.cohortNames <- getCohortNames(result)
      private$.tableCols <- availableTableColumns(result)
      private$.plotCols <- availablePlotColumns(result)
      private$.strataCols <- DrugUtilisation::strataColumns(result)
      private$.serialResult <- qs2::qs_serialize(result)

      private$.table <- Flextable$new(
        fun = DrugUtilisation::tableProportionOfPatientsCovered,
        args = list(type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )

      private$.plot <- PlotStatic$new(
        fun = DrugUtilisation::plotProportionOfPatientsCovered,
        args = list(style = "darwin"),
        parentNamespace = self$namespace,
        height = "80vh"
      )
    }
  ),

  private = list(
    .serialResult = NULL,
    .table = NULL,
    .plot = NULL,

    .cdmNames = NULL,
    .cohortNames = NULL,
    .tableCols = NULL,
    .plotCols = NULL,
    .strataCols = NULL,

    .pickerOptions = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),

    .UI = function(input, output, session) {
      shiny::fluidPage(
        private$.uiGeneralSettings(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Table",
            private$.uiTable()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.uiPlot()
          )
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
        )
      )
    },

    .uiTable = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "header"),
            label = "Header",
            choices = private$.tableCols,
            selected = c("cohort_name", private$.strataCols),
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "groupColumn"),
            label = "Group Column",
            choices = private$.tableCols,
            selected = "cdm_name",
            multiple = TRUE,
            options = private$.pickerOptions
          ),
          shiny::actionButton(
            inputId = shiny::NS(self$namespace, "submitTable"),
            label = "",
            icon = shiny::icon("refresh")
          )
        ),
        shiny::column(
          width = 10,
          shinyWidgets::addSpinner(
            private$.table$UI()
          )
        )
      )
    },

    .uiPlot = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "facet"),
            label = "facet",
            choices = private$.plotCols,
            selected = "cohort_name",
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "colour"),
            label = "colour",
            choices = private$.plotCols,
            selected = private$.strataCols,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "ribbon"),
            label = "ribbon",
            choices = c("On", "Off"),
            selected = c("Yes")
          ),
          shiny::actionButton(
            inputId = shiny::NS(self$namespace, "submitPlot"),
            label = "",
            icon = shiny::icon("refresh")
          )
        ),
        shiny::column(
          width = 10,
          shinyWidgets::addSpinner(
            private$.plot$UI()
          )
        )
      )
    },

    .server = function(input, output, session) {
      fetchData <- shiny::reactive({
        mirai::mirai({
          result |>
            qs2::qs_deserialize() |>
            dplyr::filter(.data$cdm_name %in% cdmName) |>
            omopgenerics::filterGroup(
              .data$cohort_name %in% cohortName
            )
        }, result = private$.serialResult, cdmName = input$cdmName, cohortName = input$cohortName)
      }) |>
        shiny::bindCache(input$cdmName, input$cohortName)

      shiny::observeEvent(input$submitTable, {
        private$.table$args["header"] <- list(input$header)
        private$.table$args["groupColumn"] <- list(input$groupColumn)

        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.table$args$result <- result
            private$.table$server(input, output, session)
          }
        )
      })

      shiny::observeEvent(input$submitPlot, {
        private$.plot$args["facet"] <- list(input$facet)
        private$.plot$args["colour"] <- list(input$colour)
        private$.plot$args["ribbon"] <- list(
          convertLabelToLogical(input$ribbon, trueVal = "On", falseVal = "Off")
        )

        promises::then(
          promise = fetchData(),
          onFulfilled = function(result) {
            private$.plot$args$result <- result
            private$.plot$server(input, output, session)
          }
        )
      })
    },

    .checkResult = function(result) {
      collection <- checkmate::makeAssertCollection()
      checkmate::assert_class(x = result, classes = "summarised_result", add = collection)
      checkmate::assert_true(attr(result, "settings")$result_type == "summarise_proportion_of_patients_covered", add = collection)
      checkmate::reportAssertions(collection)
    }
  )
)

source("./R/utils.R")
source("./R/utils-SummarizedResult.R")
source("./R/Table-Flextable.R")

mirai::daemons(2)

mod <- ProportionOfPatientsCovered$new(res3)

preview(mod)
