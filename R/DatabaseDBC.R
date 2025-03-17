DatabaseDBC <- R6::R6Class(
  classname = "DatabaseDBC",
  inherit = Database,

  ## Public ----
  public = list(
    ## Methods ----
    initialize = function(connectionDetails) {
      super$initialize()
      private$.connectionDetails <- connectionDetails
    },

    query = function(sql, ...) {
      DatabaseConnector::renderTranslateQuerySql(
        connection = private$.connection,
        sql = sql,
        ...
      )
    },

    execute = function(sql, ...) {
      DatabaseConnector::renderTranslateExecuteSql(
        connection = private$.connection,
        sql = sql,
        ...
      )
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .connectionDetails = NULL,

    ### Methods ----
    connect = function() {
      if (!self$connected) {
        private$.connection <- DatabaseConnector::connect(private$.connectionDetails)
        private$.reactiveValues$connected <- TRUE
        dbName <- if (file.exists(private$.connection@dbiConnection@dbname)) {
          basename(private$.connection@dbiConnection@dbname)
        } else {
          private$.connection@dbiConnection@dbname
        }
        private$.reactiveValues$databaseName <- dbName
      } else {
        message("Already connected to database")
      }
    },

    disconnect = function() {
      if (self$connected) {
        DatabaseConnector::disconnect(private$.connection)
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
