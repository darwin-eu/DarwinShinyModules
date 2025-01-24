#' @title DrugExposureDiagnostics Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' DrugExposureDiagnostics module that shows tables and plots
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
#' if (require("DrugExposureDiagnostics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
#'
#'   cdm <- DrugExposureDiagnostics:::mockDrugExposure()
#'   ded <- DrugExposureDiagnostics::executeChecks(cdm)
#'   mod <- DrugExposureDiagnostics$new(resultList = ded, databaseId = "Eunomia")
#'
#'   if (interactive()) {
#'     preview(mod)
#'   }
#' }
#' }
DrugExposureDiagnostics <- R6::R6Class(
  classname = "DrugExposureDiagnostics",
  inherit = ShinyModule,

  public = list(

    #' @description
    #' Initializer method
    #'
    #' @param resultList (`list`) List containing the output of the checks
    #' @param database_id (`character`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(resultList, database_id) {
      super$initialize()
      private$.resultList <- resultList
      private$.database_id <- database_id
      lapply(names(private$.resultList), FUN = function(name) {
        private$.resultList[[name]] <- dplyr::bind_cols(database_id = database_id,
                                                        private$.resultList[[name]])
      })
      if (nrow(private$.resultList$conceptSummary) > 0) {
        ingredientConceptsColumnsToHide <- c(
          "concept_code", "valid_start_date", "valid_end_date",
          "invalid_reason", "amount_value", "amount_unit_concept_id", "numerator_value",
          "numerator_unit_concept_id", "numerator_unit", "denominator_value",
          "denominator_unit_concept_id", "denominator_unit", "box_size", "amount_unit"
        )
        ingredientConceptColumnsSelected <- colnames(private$.resultList$conceptSummary)
        ingredientConceptColumnsSelected <- setdiff(ingredientConceptColumnsSelected, ingredientConceptsColumnsToHide)
        private$.ingredientConceptsTab <- dataPlotPanel$new(data = private$.resultList$conceptSummary,
                                                            id = "ingredientConcepts",
                                                            title = "Ingredient concepts",
                                                            description = "Ingredient concepts",
                                                            plotPercentage = FALSE,
                                                            byConcept = FALSE,
                                                            selectedColumns = ingredientConceptColumnsSelected,
                                                            downloadFilename = "IngredientConcepts.csv")
        private$.ingredientConceptsTab$parentNamespace <- self$namespace
      }
      return(invisible(self))
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .resultList = NULL,
    .database_id = NULL,
    .ingredientConceptsTab = NULL,

    ### Methods ----
    .UI = function() {
      allTabsList <- list(widths = c(2, 10))
      if (!is.null(private$.ingredientConceptsTab)) {
        allTabsList[[length(allTabsList) + 1]] <- private$.ingredientConceptsTab$uiBody()
      }
      # if (nrow(private$.resultList$drugRoutesOverall) > 0) {
      #   drugRoutes <- dataPlotPanel$new(data = private$.resultList$drugRoutesOverall,
      #                                   id = "drugRoutes",
      #                                   title = "Drug Routes",
      #                                   plotPercentage = FALSE,
      #                                   byConcept = FALSE)
      #   allTabsList[[length(allTabsList) + 1]] <- drugRoutes$uiBody()
      # }
      # if (nrow(drugTypes) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugTypes", "Drug types")
      # }
      # if (nrow(drugSourceConcepts) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugSourceConcepts", "Drug source concepts", byConcept = FALSE)
      # }
      # if (nrow(drugExposureDuration) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugExposureDuration", "Drug exposure duration")
      # }
      # if (nrow(drugVariablesMissing) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugVariablesMissing", "Drug variables missing", plotPercentage = TRUE)
      # }
      # if (nrow(drugDaysSupply) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugDaysSupply", "Drug days supply")
      # }
      # if (nrow(drugQuantity) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugQuantity", "Drug quantity")
      # }
      # if (nrow(drugSig) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugSig", "Drug sig")
      # }
      # if (nrow(drugVerbatimEndDate) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugVerbatimEndDate", "Drug verbatim end date")
      # }
      # if (nrow(drugDailyDose) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- dataPlotPanelViewer("drugDailyDose", "Drug daily dose")
      # }
      # if (nrow(metaData) > 0) {
      #   allTabsList[[length(allTabsList) + 1]] <- metaDataViewer("metaData", "Metadata")
      # }
      shiny::fluidPage(
        theme = bslib::bs_theme(version = "5", bootswatch = "spacelab"),
        shinyjs::useShinyjs(),
        shiny::titlePanel(
          title = h2("Drug Exposure Diagnostics Dashboard", align = "center"),
          windowTitle = "Drug Exposure Diagnostics Dashboard"
        ),
        do.call(navlistPanel, allTabsList)
      )
    },

    .server = function(input, output, session) {
      private$.ingredientConceptsTab$server(input, output, session)
    },

    assertInstall = function() {
      if (!require("DrugExposureDiagnostics", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
        answer <- readline(prompt = "`DrugExposureDiagnostics` is not installed, would you like to install from CRAN? (y/n)")
        if (substr(tolower(answer), start = 1, stop = 1) == "y") {
          utils::install.packages("DrugExposureDiagnostics")
        } else if (substr(tolower(answer), start = 1, stop = 1) == "n") {
          stop("You can install `DrugExposureDiagnostics` manually by running one of the following:\n  1. `install.packages('DrugExposureDiagnostics')`\n  2. `remotes::install_github('darwin-eu/DrugExposureDiagnostics')`")
        } else {
          stop("Your answer was not `y` or `n`")
        }
      }
    }
  )
)
