#' @title DatabaseDBI Class
#'
#' @include Database.R
#'
#' @description
#' This class extends the `Database` class, to query a database using
#' `DBI` with ODBC.
#'
#' @export
DatabaseDBI <- R6::R6Class(
  classname = "DatabaseDBI",
  inherit = Database,

  ## Active ----
  active = list(
    #' @field connection (`DBI Connection`) A DBI connection.
    connection = function() {
      return(private$.connection)
    },
    driver = function() {
      return(private$.driver)
    },

    #' @field connectArgs (`list(n)`) Named list of additional arguments used in `DBI::dbConnect()`
    connectArgs = function(connectArgs) {
      if (missing(connectArgs)) {
        return(private$.connectArgs)
      } else {
        checkmate::assertList(connectArgs, names = "named")
        private$.connectArgs <- connectArgs
      }
    },

    #' @field disconnectArgs (`list(n)`) Named list of additional arguments used in `DBI::dbDisconnect()`
    disconnectArgs = function(disconnectArgs) {
      if (missing(disconnectArgs)) {
        return(private$.disconnectArgs)
      } else {
        checkmate::assertList(disconnectArgs, names = "named")
        private$.disconnectArgs <- disconnectArgs
      }
    }
  ),

  ## Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @param driver Driver to use to connect to the database with `DBI::dbConnect()`
    #'
    #' @return `invisible(self)`
    initialize = function(driver) {
      rlang::check_installed("DBI")
      super$initialize()
      private$.driver <- driver
      return(invisible(self))
    },

    #' @description
    #' Method to connect to the database.
    connect = function() {
      if (!self$connected) {
        private$.connection <- do.call(
          DBI::dbConnect,
          args = append(list(drv = private$.driver), private$.connectArgs)
        )
        private$.reactiveValues$connected <- TRUE
        dbName <- tryCatch(
          {
            private$.connection@dbname
          },
          error = function(e) {
            basename(private$.connection@driver@dbdir)
          }
        )
        private$.reactiveValues$databaseName <- dbName
      }
    },

    #' @description
    #' Method to disconnect from the database.
    disconnect = function() {
      if (self$connected) {
        do.call(
          DBI::dbDisconnect,
          args = append(list(conn = private$.connection), private$.disconnectArgs)
        )
        private$.connection <- NULL
        private$.reactiveValues$connected <- FALSE
        private$.reactiveValues$databaseName <- ""
        message("Disconnected from database")
      }
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .driver = NULL,
    .connectArgs = list(),
    .disconnectArgs = list()
  )
)
