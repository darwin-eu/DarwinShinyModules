TPDataInterface <- R6::R6Class(
  classname = "TPDataInterface",
  inherit = ShinyModule,

  # Active ----
  active = list(
    treatment_pathways = function() {
      private$.tables$treatment_pathways <- private$.tables$treatment_pathways |>
        private$checkConnection()
      return(private$.tables$treatment_pathways)
    },
    cdm_source_info = function() {
      private$.tables$cdm_source_info <- private$.tables$cdm_source_info |>
        private$checkConnection()
      return(private$.tables$cdm_source_info)
    },
    database = function() {
      return(private$.database)
    }
  ),

  # Public ----
  public = list(
    initialize = function(..., overwrite = TRUE) {
      super$initialize()
      private$.DB <- file.path(tempdir(), "tp_mod.duckdb")

      private$.database <- DarwinShinyModules::DatabaseDBI$new(
        driver = duckdb::duckdb(dbdir = private$.DB)
      )
      private$.database$parentNamespace <- self$namespace

      dots <- list(...)
      private$loadResults(dots)
    }
  ),

  # Private ----
  private = list(
    ## Constants ----
    .DB = "",
    .FILENAMES = c(
      "attrition.csv", "metadata.csv", "treatment_pathways.csv",
      "summary_event_duration.csv", "counts_age.csv", "counts_sex.csv",
      "counts_year.csv", "cdm_source_info.csv", "analyses.csv",
      "arguments.csv"
    ),

    ## Nested Modules ----
    .database = NULL,

    ## Fields ----
    .nResults = 0,
    .tables = NULL,

    ## Overrides ----
    .UI = function() {
      private$.database$UI()
    },
    .server = function(input, output, session) {
      private$.database$server(input, output, session)
    },

    ## Methods ----
    finalize = function() {
      private$.database$disconnect()
      unlink(private$.DB, recursive = TRUE, force = TRUE)
    },
    checkConnection = function(tbl) {
      if (private$.database$connected) {
        if (!identical(private$.database$connection, tbl$src$con)) {
          tbl$src$con <- private$.database$connection
        }
      }
      return(tbl)
    },
    loadResults = function(dots) {
      private$.database$connect()
      private$.tables <- new.env()
      for (arg in dots) {
        private$.nResults <- private$.nResults + 1
        if ("TreatmentPatternsResults" %in% class(arg)) {
          private$loadFromTPR(arg)
        } else if (dir.exists(arg)) {
          private$loadFromCSV(arg)
        } else if (file.exists(arg)) {
          private$loadFromZIP(arg)
        }
      }
    },
    getLabel = function(file) {
      substr(file, start = 1, stop = nchar(file) - 4)
    },
    loadFromTPR = function(tpr) {
      for (i in seq_len(length(private$.FILENAMES))) {
        file <- private$.FILENAMES[i]
        label <- private$getLabel(file)
        private$uploadFiles(
          data = tpr[[label]],
          label = label
        )
      }
    },
    loadFromCSV = function(dir) {
      for (i in seq_len(length(private$.FILENAMES))) {
        file <- private$.FILENAMES[i]
        label <- private$getLabel(file)
        private$uploadFiles(
          data = read.csv(file.path(dir, file)),
          label = label
        )
      }
    },
    loadFromZIP = function(zipFile) {
      tempDir <- file.path(tempdir(), "tp-zip")
      unzip(zipFile, exdir = tempDir)
      private$loadFromCSV(tempDir)
    },
    uploadFiles = function(data, label) {
      data <- data |>
        dplyr::mutate(result_id = as.integer(private$.nResults))

      if (is.null(private$.tables[[label]])) {
        dplyr::copy_to(dest = private$.database$connection, df = data, name = label, overwrite = TRUE, temporary = FALSE)
        private$.tables[[label]] <- dplyr::tbl(private$.database$connection, label)
      } else {
        DBI::dbAppendTable(conn = private$.database$connection, name = label, value = data)
      }
    }
  )
)
