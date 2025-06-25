#' @title DatabaseDBC Class
#'
#' @include Database.R
#'
#' @description
#' This class extends the `Database` class, to query a database using
#' `DatabaseConnector` with JDBC.
#'
#' @export
DatabaseDBC <- R6::R6Class(
  classname = "DatabaseDBC",
  inherit = Database,

  ## Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @param connectionDetails (`ConnectionDetails`) Connection Details from `DatabaseConnector::createConnectionDetails`
    #'
    #' @return `invisible(self)`
    initialize = function(connectionDetails) {
      rlang::check_installed("DatabaseConnector")
      super$initialize()
      private$.connectionDetails <- connectionDetails
      return(invisible(self))
    },

    #' @description
    #' Query method to query the database using `DatabaseConnector::renderTranslateQuerySql`
    #'
    #' @param sql (`character(1)`) SQL Query
    #' @param ... Additional parameters for `DatabaseConnector::renderTranslateQuerySql`
    #'
    #' @return `data.frame`
    query = function(sql, ...) {
      DatabaseConnector::renderTranslateQuerySql(
        connection = private$.connection,
        sql = sql,
        ...
      )
    },

    #' @description
    #' Execute method to execute a query on the database using `DatabaseConnector::renderTranslateExecuteSql`
    #'
    #' @param sql (`character(1)`) SQL Query to execute
    #' @param ... Additional parameters for `DatabaseConnector::renderTranslateExecuteSql`
    #'
    #' @return `invisible(self)`
    execute = function(sql, ...) {
      DatabaseConnector::renderTranslateExecuteSql(
        connection = private$.connection,
        sql = sql,
        ...
      )
      return(invisible(self))
    },

    #' @description
    #' Method to upload data to the database
    #'
    #' @param tableName (`character(1)`) Name of the table
    #' @param data (`data.frame`) data.frame like table to upload
    uploadTable = function(tableName, data) {
      DatabaseConnector::dbWriteTable(conn = private$.connection, name = tableName, value = data)
    },

    #' @description
    #' Method to connect to the database.
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
      }
    },

    #' @description
    #' Method to disconnect from the database.
    disconnect = function() {
      if (self$connected) {
        DatabaseConnector::disconnect(private$.connection)
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
    .connectionDetails = NULL
  )
)
