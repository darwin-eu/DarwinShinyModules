#' @title OhdsiModule Class
#'
#' @description
#' The OhdsiModule wraps around a `viewerX()` and `serverX()` function from
#' `OhdsiShinyModules`, running the module stand-alone. This setup should also
#' support any module from `OhdsiShinyModules`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DarwinShinyModules)
#' library(DatabaseConnector)
#' library(ResultModelManager)
#' library(OhdsiShinyModules)
#' library(ShinyAppBuilder)
#'
#' connectionDetails <- createConnectionDetails(
#'   dbms = "sqlite",
#'   server = "./results.sqlite"
#' )
#'
#' connectionHandler <- ConnectionHandler$new(
#'   connectionDetails = connectionDetails
#' )
#'
#' estimation <- OhdsiModule$new(
#'   connectionHandler = connectionHandler,
#'   viewerFun = estimationViewer,
#'   serverFun = estimationServer,
#'   resultDatabaseSettings = createDefaultResultDatabaseSettings()
#' )
#'
#' if (interactive()) {
#'   preview(est)
#' }
#' }
OhdsiModule <- R6::R6Class(
  classname = "OhdsiModule",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field connectionHandler (`ConnectionHandler`) ConnectionHandler object from `ResultModelManager`.
    connectionHandler = function() {
      return(private$.connectionHandler)
    },

    #' @field viewerFun (`function`) Viewer function to use from `OhdsiShinyModules`.
    viewerFun = function() {
      return(private$.viewerFun)
    },

    #' @field serverFun (`function`) Server function to use from `OhdsiShinyModules`.
    serverFun = function() {
      return(private$.serverFun)
    },

    #' @field resultDatabaseSettings (`list`) Named List of table prefixes like `ShinyAppBuilder::createDefaultResultDatabaseSettings()` creates.
    resultDatabaseSettings = function() {
      return(private$.resultDatabaseSettings)
    }
  ),

  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param connectionHandler (`ConnectionHandler`) ConnectionHandler object from `ResultModelManager`.
    #' @param viewerFun (`function`) Viewer function to use from `OhdsiShinyModules`.
    #' @param serverFun (`function`) Server function to use from `OhdsiShinyModules`.
    #' @param resultDatabaseSettings (`list`) Named List of table prefixes like `ShinyAppBuilder::createDefaultResultDatabaseSettings()` creates.
    #'
    #' @returns `self`
    initialize = function(connectionHandler, viewerFun, serverFun, resultDatabaseSettings = ShinyAppBuilder::createDefaultResultDatabaseSettings()) {
      private$.connectionHandler <- connectionHandler
      private$.viewerFun <- viewerFun
      private$.serverFun <- serverFun
      private$.resultDatabaseSettings <- resultDatabaseSettings
      return(self)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .connectionHandler = NULL,
    .viewerFun = NULL,
    .serverFun = NULL,
    .resultDatabaseSettings = list(),

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        do.call(
          what = private$.viewerFun,
          args = list(
            id = sprintf("%s-ohdsi", self$namespace)
          )
        )
      )
    },

    .server = function(input, output, session) {
      do.call(
        what = private$.serverFun,
        args = list(
          id = "ohdsi",
          connectionHandler = private$.connectionHandler,
          resultDatabaseSettings = private$.resultDatabaseSettings
        )
      )
    }
  )
)
