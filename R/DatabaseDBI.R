DatabaseDBI <- R6::R6Class(
  classname = "DatabaseDBI",
  inherit = Database,

  ## Active ----
  active = list(
    tables = function() {
      return(private$.tables)
    },

    connectArgs = function(connectArgs) {
      if (missing(connectArgs)) {
        return(private$.connectArgs)
      } else {
        checkmate::assertList(connectArgs, names = "named")
        private$.connectArgs <- connectArgs
      }
    },

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
    initialize = function(driver) {
      super$initialize()
      private$.driver <- driver
    },

    attachTables = function(...) {
      tableNames <- c(...)
      private$.tables <- lapply(tableNames, function(table) {
        dplyr::tbl(src = private$.connection, table)
      })
      names(private$.tables) <- tableNames
    },

    detatchTables = function(...) {
      tableNames <- c(...)
      private$.tables <- private$.tables[[tableNames]]
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .driver = NULL,
    .tables = list(),
    .connectArgs = list(),
    .disconnectArgs = list(),

    ### Methods ----
    connect = function() {
      if (!self$connected) {
        private$.connection <- do.call(
          DBI::dbConnect,
          args = append(list(drv = private$.driver), private$.connectArgs)
        )
        private$.reactiveValues$connected <- TRUE
        dbName <- tryCatch({
          private$.connection@dbname
        }, error = function(e) {
          basename(private$.connection@driver@dbdir)
        })
        private$.reactiveValues$databaseName <- dbName
      } else {
        message("Already connected to database")
      }
    },

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
      } else {
        message("Already disconnected from database")
      }
    }
  )
)
