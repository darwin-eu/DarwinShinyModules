# Copyright 2025 DARWIN EU®
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

#' @title TreatmentPathways
#'
#' @description
#' Module that displays the Treatment Pathways from the `TreatmentPatterns` package.
#'
#' @export
#'
#' @examples{
#'   if (interactive()) {
#'     library(DarwinShinyModules)
#'
#'     tpr <- TreatmentPatterns::TreatmentPatternsResults$new(
#'       filePath = "./inst/dummyData/TreatmentPatterns/3.0.0"
#'     )
#'
#'     treatmentPathways <- TreatmentPathways$new(
#'       treatmentPathways = tpr$treatment_pathways,
#'       dmSourceInfo = tpr$cdm_source_info
#'     )
#'
#'     preview(treatmentPathways)
#'   }
#' }
#'
TreatmentPathways <- R6::R6Class(
  classname = "TreatmentPathways",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field colours (`list`) Hex colour values used in the Sunburst Plots and
    #' Sankey Diagrams.
    colours = function() {
      return(private$.colours)
    },

    #' @field sunburst (`PlotWidget`) Module.
    sunburst = function() {
      return(private$.sunburst)
    },

    #' @field sankey (`PlotWidget`) Module.
    sankey = function() {
      return(private$.sankey)
    },

    #' @field inputPanel (`InputPanel`) Module.
    inputPanel = function() {
      return(private$.inputPanel)
    },

    #' @field table Table displaying the `treatment_pathways` csv-file.
    table = function() {
      return(private$.table)
    },

    #' @field sunburstOverview (`list`) Containing Sunburst `PlotWidget` modules.
    sunburstOverview = function() {
      return(private$.sunburstOverview)
    },

    #' @field treatmentPathways (`data.frame`)
    treatmentPathways = function(treatmentPathways) {
      if (missing(treatmentPathways)) {
        return(private$.treatmentPathways)
      } else {
        private$.treatmentPathways <- treatmentPathways
      }
    },

    #' @field cdmSourceInfo (`data.frame`)
    cdmSourceInfo = function(cdmSourceInfo) {
      if (missing(cdmSourceInfo)) {
        return(private$.cdmSourceInfo)
      } else {
        private$.cdmSourceInfo <- cdmSourceInfo
      }
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param treatmentPathways (`data.frame`) `treatment_pathways` field from the `TreatmentPatternsResult` object.
    #' @param cdmSourceInfo (`data.frame`) `cdm_source_info` field from the `TreatmentPatternsResult` object.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(treatmentPathways, cdmSourceInfo, ...) {
      super$initialize(...)
      private$assertInstall("TreatmentPatterns", "3.0.0")
      private$.treatmentPathways <- treatmentPathways
      private$.cdmSourceInfo <- cdmSourceInfo

      private$setColours()
      private$initSankey()
      private$initSunburst()
      private$initInputPanel()
      private$initTable()

      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .colours = NULL,
    .treatmentPathways = NULL,
    .cdmSourceInfo = NULL,

    ## Nested Modules ----
    .sunburst = NULL,
    .sankey = NULL,
    .inputPanel = NULL,
    .table = NULL,
    .sunburstOverview = list(),

    ## Overrided ----
    .UI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 10,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Overview",
              shiny::htmlOutput(outputId = shiny::NS(self$namespace, id = "overview"))
            ),
            shiny::tabPanel(
              title = "Detail",
              shiny::tabsetPanel(
                shiny::tabPanel(
                  title = "Sunburst",
                  private$.sunburst$UI()
                ),
                shiny::tabPanel(
                  title = "Sankey",
                  private$.sankey$UI()
                )
              ),
              private$.table$UI()
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.sunburst$server(input, output, session)
      private$.sankey$server(input, output, session)
      private$.table$server(input, output, session)

      cdmSourceInfo <- renameDatabases(private$.cdmSourceInfo)

      private$updatePickers(cdmSourceInfo, session)

      shiny::observeEvent(
        list(
          private$.inputPanel$inputValues$database,
          private$.inputPanel$inputValues$targetCohort,
          private$.inputPanel$inputValues$age,
          private$.inputPanel$inputValues$sex,
          private$.inputPanel$inputValues$indexYear,
          private$.inputPanel$inputValues$minFreq,
          private$.inputPanel$inputValues$groupCombinations
        ),
        {
          private$.sunburst$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations
          private$.sankey$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations

          overviewDat <- private$filterOverview(cdmSourceInfo)
          private$createSunbursts(overviewDat)
          private$renderOverview(output)

          detailDat <- private$filterDetail(cdmSourceInfo)
          private$.sunburst$args$treatmentPathways <- detailDat
          private$.sankey$args$treatmentPathways <- detailDat
          private$.table$reactiveValues$data <- detailDat
        }
      )
    },

    ## Methods ----
    filterOverview = function(cdmSourceInfo) {
      cdmSourceInfo |>
        dplyr::inner_join(
          private$.treatmentPathways,
          by = dplyr::join_by(
            analysis_id == analysis_id
          ), copy = TRUE
        ) |>
        dplyr::filter(
          .data$target_cohort_name == private$.inputPanel$inputValues$targetCohort,
          .data$age == private$.inputPanel$inputValues$age,
          .data$sex == private$.inputPanel$inputValues$sex,
          .data$index_year == private$.inputPanel$inputValues$indexYear,
          .data$freq >= private$.inputPanel$inputValues$minFreq
        ) |>
        dplyr::select(
          "pathway",
          "freq",
          "age",
          "sex",
          "index_year",
          "analysis_id",
          "target_cohort_id",
          "target_cohort_name",
          "cdm_source_abbreviation"
        ) |>
        dplyr::collect()
      # }
    },
    createSunbursts = function(data) {
      # if (!is.null(data)) {
      dfs <- data |>
        dplyr::group_by(.data$cdm_source_abbreviation) |>
        dplyr::group_split()

      private$.sunburstOverview <- lapply(dfs, function(df) {
        mod <- PlotWidget$new(
          fun = TreatmentPatterns::createSunburstPlot,
          args = list(
            treatmentPathways = df,
            groupCombinations = private$.inputPanel$inputValues$groupCombinations,
            colors = list(
              domain = names(private$.colours),
              range = as.character(private$.colours)
            ),
            legend = FALSE,
            height = "70vh",
            width = "40vw"
          ),
          title = unique(df$cdm_source_abbreviation)
        )
        mod$parentNamespace <- self$namespace
        mod$async <- TRUE
        return(mod)
      })
      # }
    },
    renderOverview = function(output) {
      uis <- lapply(private$.sunburstOverview, function(mod) {
        mod$server(input, output, session)
        mod$UI()
      })

      output$overview <- shiny::renderUI({
        shiny::fluidPage(
          lapply(seq(1, length(uis), 2), function(i) {
            shiny::fluidRow(
              lapply(list(uis[i], uis[i + 1]), function(ui) {
                shiny::column(ui, width = 6)
              })
            )
          })
        )
      })
    },
    filterDetail = function(cdmSourceInfo) {
      cdmSourceInfo |>
        dplyr::inner_join(
          private$.treatmentPathways,
          by = dplyr::join_by(
            analysis_id == analysis_id
          ), copy = TRUE
        ) |>
        dplyr::filter(
          .data$target_cohort_name %in% private$.inputPanel$inputValues$targetCohort,
          .data$cdm_source_abbreviation == private$.inputPanel$inputValues$database,
          .data$age == private$.inputPanel$inputValues$age,
          .data$sex == private$.inputPanel$inputValues$sex,
          .data$index_year == private$.inputPanel$inputValues$indexYear,
          .data$freq >= private$.inputPanel$inputValues$minFreq
        ) |>
        dplyr::select(
          "pathway",
          "freq",
          "age",
          "sex",
          "index_year",
          "analysis_id",
          "target_cohort_id",
          "target_cohort_name"
        ) |>
        dplyr::collect()
      # }
    },
    initInputPanel = function() {
      databaseLabels <- private$.cdmSourceInfo |>
        dplyr::pull(.data$cdm_source_abbreviation) |>
        unique()

      targets <- private$.treatmentPathways |>
        dplyr::pull(.data$target_cohort_name) |>
        unique()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          database = shinyWidgets::pickerInput,
          targetCohort = shinyWidgets::pickerInput,
          age = shinyWidgets::pickerInput,
          sex = shinyWidgets::pickerInput,
          indexYear = shinyWidgets::pickerInput,
          minFreq = shiny::sliderInput,
          groupCombinations = shiny::checkboxInput
        ),
        args = list(
          database = list(
            inputId = shiny::NS(self$namespace, "database"),
            label = "Database",
            choices = databaseLabels,
            selected = databaseLabels[1],
            multiple = FALSE
          ),
          targetCohort = list(
            inputId = shiny::NS(self$namespace, "targetCohort"),
            label = "Target Cohort",
            choices = targets,
            selected = targets[1],
            multiple = FALSE
          ),
          age = list(
            inputId = shiny::NS(self$namespace, "age"),
            label = "Age",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$age) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          sex = list(
            inputId = shiny::NS(self$namespace, "sex"),
            label = "Sex",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$sex) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          indexYear = list(
            inputId = shiny::NS(self$namespace, "indexYear"),
            label = "Index Year",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$index_year) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          minFreq = list(
            inputId = shiny::NS(self$namespace, "minFreq"),
            label = "Minimum Frequency",
            min = private$.treatmentPathways |>
              dplyr::pull(.data$freq) |>
              min(),
            max = private$.treatmentPathways |>
              dplyr::pull(.data$freq) |>
              max(),
            value = private$.treatmentPathways |>
              dplyr::pull(.data$freq) |>
              min()
          ),
          groupCombinations = list(
            inputId = shiny::NS(self$namespace, "groupCombinations"),
            label = "Group Combinations",
            value = FALSE
          )
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace,
        async = TRUE
      )
    },
    fetchColours = function(ncolor, s = 0.5, v = 0.95, seed = 40) {
      # From: https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r/61183611#61183611
      golden_ratio_conjugate <- 0.618033988749895
      set.seed(seed)
      h <- runif(1)
      H <- vector("numeric", ncolor)
      for (i in seq_len(ncolor)) {
        h <- (h + golden_ratio_conjugate) %% 1
        H[i] <- h
      }
      as.list(hsv(H, s = s, v = v))
    },
    setColours = function() {
      # if (is.null(private$.colours)) {
      nodes <- private$.treatmentPathways |>
        dplyr::pull(.data$pathway) |>
        stringr::str_split(pattern = "-") |>
        unlist() |>
        unique()

      nodes <- if ("None" %in% nodes) {
        c(nodes, "Stopped", "Combination")
      } else {
        c(nodes, "None", "Stopped", "Combination")
      }

      private$.colours <- private$fetchColours(length(nodes))
      names(private$.colours) <- nodes
      # }
    },
    initSankey = function() {
      private$.sankey <- PlotWidget$new(
        fun = TreatmentPatterns::createSankeyDiagram,
        args = list(
          colors = private$.colours,
          fontSize = 12,
          height = "40vw",
          width = "50vw"
        ),
        title = NULL,
        parentNamespace = self$namespace,
        async = TRUE
      )
    },
    initSunburst = function() {
      # Drop "Stopped"
      private$.sunburst <- PlotWidget$new(
        fun = TreatmentPatterns::createSunburstPlot,
        args = list(
          colors = list(
            domain = names(private$.colours),
            range = as.character(private$.colours)
          ),
          legend = list(w = 200),
          height = "70vh",
          width = "50vw"
        ),
        title = NULL,
        parentNamespace = self$namespace,
        async = TRUE
      )
    },
    initTable = function() {
      private$.table <- Table$new(
        data = NULL,
        title = NULL,
        filter = "none",
        parentNamespace = self$namespace,
        async = TRUE
      )
    },
    updatePickers = function(cdmSourceInfo, session) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(private$.inputPanel$moduleId, "database"),
        choices = cdmSourceInfo$cdm_source_abbreviation
      )
    }
  )
)
