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

#' @title TreatmentPatterns Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' TreatmentPatterns module that shows a Sunburst plot and Sankey diagram, with
#' a table.
#'
#' @details
#' The module consists of the following:
#' \describe{
#'   \item{"InputPanel"}{Input panel to filter data.}
#'   \item{"PlotWidget"}{Sunburst Plot, visualizing the data.}
#'   \item{"PlotWidget"}{Sankey Diagram, visualizing the data.}
#'   \item{"Table"}{Table containing the data.}
#' }
#'
#' @export
#'
#' @examples{
#' library(DarwinShinyModules)
#'
#' if (require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
#'
#'   tp <- data.frame(
#'     path = c("A+B-C", "B+C-A", "B-A+C"),
#'     freq = 100,
#'     sex = "all",
#'     age = "all",
#'     indexYear = "all"
#'   )
#'
#'   treatmentPathways <- TreatmentPatterns$new(treatmentPathways = tp)
#'
#'   if (interactive()) {
#'     preview(treatmentPathways)
#'   }
#' }
#' }
TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  active = list(
    #' @field sankeyCols (`list(a = "#ff33cc")`) Colours for the Sankey diagram.
    sankeyCols = function(sankeyCols) {
      if (missing(sankeyCols)) {
        return(private$.sankeyCols)
      } else {
        checkmate::assertList(sankeyCols, types = "character")
        private$.sankeyCols <- sankeyCols
      }
    },

    #' @field sunburstCols (`list(domain = list(), range = list())`) Colours for the Sunburst plot.
    sunburstCols = function(sunburstCols) {
      if (missing(sunburstCols)) {
        return(private$.sunburstCols)
      } else {
        assertCol <- checkmate::makeAssertCollection()
        checkmate::assertList(sunburstCols, types = "list", len = 2, add = assertCol)
        checkmate::assertList(sunburstCols[[1]], types = "character", add = assertCol)
        checkmate::assertList(sunburstCols[[2]], types = "character", add = assertCol)
        checkmate::reportAssertions(assertCol)
        private$.sunburstCols <- sunburstCols
      }
    }
  ),

  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param treatmentPathways (`data.frame`) Contents of the treatmentPathways.csv file from the `export()` function of TreatmentPatterns.
    #'
    #' @return (`invisible(self)`)
    initialize = function(treatmentPathways) {
      private$.treatmentPathways <- treatmentPathways

      colNames <- colnames(treatmentPathways)
      private$.tpColRef <- list(
        pathway = colNames[grep("path", colNames)],
        frequency = colNames[grep("freq", colNames)],
        age = colNames[grep("age", colNames)],
        sex = colNames[grep("sex", colNames)],
        indexYear = colNames[grep("index(_)?[Yy]ear", colNames)],
        analysisId = colNames[grep("analysis_id", colNames)],
        targetCohortId = colNames[grep("target_cohort_id", colNames)],
        targetCohortName = colNames[grep("target_cohort_name", colNames)]
      )

      super$initialize()
      private$initInputPanel()

      private$.sunburst <- PlotWidget$new(
        fun = TreatmentPatterns::createSunburstPlot,
        args = list(treatmentPathways = treatmentPathways),
        title = NULL
      )
      private$.sunburst$parentNamespace <- self$namespace

      private$.sankey <- PlotWidget$new(
        fun = TreatmentPatterns::createSankeyDiagram,
        args = list(treatmentPathways = treatmentPathways),
        title = NULL
      )
      private$.sankey$parentNamespace <- self$namespace

      private$.table <- Table$new(data = treatmentPathways, title = NULL, filter = "none")
      private$.table$parentNamespace <- self$namespace
      return(invisible(self))
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .treatmentPathways = NULL,
    .inputPanel = NULL,
    .sunburst = NULL,
    .sankey = NULL,
    .table = NULL,
    .sunburstCols = NULL,
    .sankeyCols = NULL,
    .tpColRef = list(),

    ### Methods ----
    .UI = function() {
      shiny::wellPanel(
        shiny::column(
          width = 4,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 8,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Sunburst Plot",
              private$.sunburst$UI()
            ),
            shiny::tabPanel(
              title = "Sankey Diagram",
              private$.sankey$UI()
            )
          )
        ),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.sunburst$server(input, output, session)
      private$.sankey$server(input, output, session)
      private$.table$server(input, output, session)
      observeEvent(
        c(
          private$.inputPanel$inputValues$none,
          private$.inputPanel$inputValues$groupCombi,
          private$.inputPanel$inputValues$ageGroup,
          private$.inputPanel$inputValues$sexGroup,
          private$.inputPanel$inputValues$yearGroup
        ), {
          dataUpdated <- private$updateData(private$.treatmentPathways)
          # private$setColours(dataUpdated)
          private$updateTable(dataUpdated)
          private$updateSunburst(dataUpdated)
          private$updateSankey(dataUpdated)
        })
    },

    assertInstall = function() {
      if (!require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        answer <- readline(prompt = "`TreatmentPatterns` is not installed, would you like to install from CRAN? (y/n)")
        if (substr(tolower(answer), start = 1, stop = 1) == "y") {
          utils::install.packages("TreatmentPatterns")
        } else if (substr(tolower(answer), start = 1, stop = 1) == "n") {
          stop("You can install `TreatmentPatterns` manually by running one of the following:\n  1. `install.packages('TreatmentPatterns')`\n  2. `remotes::install_github('darwin-eu/TreatmentPatterns')`")
        } else {
          stop("Your answer was not `y` or `n`")
        }
      }
    },

    updateTable = function(data) {
      private$.table$data <- data %>%
        dplyr::arrange(dplyr::desc(.data$freq))
    },

    updateSunburst = function(data) {
      private$.sunburst$args$groupCombinations <- private$.inputPanel$inputValues$groupCombi
      private$.sunburst$args$legend <- list(w = 400)
      private$.sunburst$args$colors <- private$.sunburstCols
      private$.sunburst$args$treatmentPathways <- data
    },

    updateSankey = function(data) {
      private$.sankey$args$groupCombinations <- private$.inputPanel$inputValues$groupCombi
      private$.sankey$args$colors <- private$.sankeyCols
      private$.sankey$args$treatmentPathways <- data
    },

    updateData = function(data) {
      none <- private$getNone()
      data <- data %>%
        dplyr::filter(
          .data[[private$.tpColRef$pathway]] != none,
          .data$age == private$.inputPanel$inputValues$ageGroup,
          .data$sex == private$.inputPanel$inputValues$sexGroup,
          .data[[private$.tpColRef$indexYear]] == private$.inputPanel$inputValues$yearGroup
        )
      return(data)
    },

    setColours = function(data) {
      if (is.null(private$.sunburstCols) & is.null(private$.sankeyCols)) {
        colors <- c(
          "#30123BFF", "#341A4EFF", "#362160FF", "#392971FF", "#3B3082FF",
          "#28BDEAFF", "#23C3E4FF", "#1FC8DEFF", "#1CCDD8FF", "#1AD3D1FF",
          "#A6FC3AFF", "#ADFB38FF", "#B4F836FF", "#BBF535FF", "#C1F334FF",
          "#FA7A1FFF", "#F8731CFF", "#F66B19FF", "#F36315FF", "#F05C13FF",
          "#3E3890FF", "#3F3F9FFF", "#4146ACFF", "#424DB8FF", "#4454C4FF",
          "#18D8C9FF", "#18DDC2FF", "#18E1BCFF", "#1AE4B6FF", "#1EE8B0FF",
          "#C7EF34FF", "#CEEC34FF", "#D4E735FF", "#DAE336FF", "#DFDF37FF",
          "#ED5510FF", "#EA4F0DFF", "#E7490CFF", "#E2430AFF", "#DE3E08FF",
          "#455BCEFF", "#4662D7FF", "#4669E0FF", "#4770E8FF", "#4776EEFF",
          "#22EBAAFF", "#29EFA2FF", "#30F19AFF", "#38F491FF", "#41F689FF",
          "#E4DA38FF", "#E9D539FF", "#EDD03AFF", "#F1CA3AFF", "#F5C53AFF",
          "#DA3907FF", "#D53406FF", "#D02F05FF", "#CB2A04FF", "#C42603FF",
          "#467CF3FF", "#4683F8FF", "#4589FCFF", "#4490FEFF", "#4196FFFF",
          "#4AF880FF", "#54FA78FF", "#5EFC6EFF", "#68FD67FF", "#72FE5EFF",
          "#F7C03AFF", "#FABA39FF", "#FCB436FF", "#FDAE35FF", "#FEA832FF",
          "#BE2102FF", "#B81E02FF", "#B21A01FF", "#AA1701FF", "#A41301FF",
          "#3D9DFEFF", "#3AA3FCFF", "#36AAF9FF", "#30B0F5FF", "#2CB7F0FF",
          "#7DFF56FF", "#86FF50FF", "#8FFF49FF", "#98FE43FF", "#9FFD3FFF",
          "#FEA030FF", "#FE992CFF", "#FE922AFF", "#FD8A26FF", "#FB8222FF",
          "#9C0F01FF", "#940C01FF", "#8B0902FF", "#830702FF", "#7A0403FF"
        )

        labels <- data %>%
          dplyr::pull(.data[[private$.tpColRef$pathway]]) %>%
          strsplit(split = "-") %>%
          unlist() %>%
          unique()

        cols <- if (length(labels) > 0 & length(labels) <= 10) {
          as.list(colors[seq(length(labels)) * 5 - 4])
        } else if (length(labels) >= 11 & length(labels) <= 20) {
          list(
            colors[seq(length(labels)) * 5 - 4],
            colors[seq(length(labels)) * 5]
          )
        } else if (length(labels) >= 21 & length(labels) <= 30) {
          list(
            colors[seq(length(labels)) * 5 - 4],
            colors[seq(length(labels)) * 5 - 2],
            colors[seq(length(labels)) * 5]
          )
        } else {
          as.list(colors[seq(length(labels))])
        }

        private$.sunburstCols <- list(domain = as.list(labels), range = cols)
        names(cols) <- labels
        private$.sankeyCols <- cols
      }
    },

    getNone = function() {
      if (private$.inputPanel$inputValues$none) {
        return("")
      } else {
        return("None")
      }
    },

    initInputPanel = function() {
      private$.inputPanel <- InputPanel$new(
        funs = list(
          none = shiny::checkboxInput,
          groupCombi = shiny::checkboxInput,
          ageGroup = shinyWidgets::pickerInput,
          sexGroup = shinyWidgets::pickerInput,
          yearGroup = shinyWidgets::pickerInput
        ),
        args = list(
          none = list(
            inputId = "none",
            label = "Show none paths",
            value = TRUE
          ),
          groupCombi = list(
            inputId = "groupCombi",
            label = "Group Combinations",
            value = TRUE
          ),
          ageGroup = list(
            inputId = "ageGroup",
            choices = unique(private$.treatmentPathways$age),
            label = "Age Group"
          ),
          sexGroup = list(
            inputId = "sexGroup",
            choices = unique(private$.treatmentPathways$sex),
            label = "Sex"
          ),
          yearGroup = list(
            inputId = "yearGroup",
            choices = unique(private$.treatmentPathways[[private$.tpColRef$indexYear]]),
            label = "Index Year"
          )
        )
      )
      private$.inputPanel$parentNamespace <- self$namespace
    }
  )
)
