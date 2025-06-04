TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  # Active ----
  active = list(),

  # Publc ----
  public = list(
    #' @description Initializer method
    #'
    #' @param ... Either 1) A directory containing csv-files from
    #' TreatmentPatterns; 2) A zip-file containing csv-files from
    #' TreatmentPatterns; or 3) A `TreatmentPatternsResults` R6 class from
    #' TreatmentPatterns.
    initialize = function(...) {
      super$initialize()
      dots <- list(...)
      private$loadResults(dots)
    }
  ),

  # Private ----
  private = list(
    ## Constant ----
    .FILENAMES = c(
      "attrition.csv", "metadata.csv", "treatment_pathways.csv",
      "summary_event_duration.csv", "counts_age.csv", "counts_sex.csv",
      "counts_year.csv", "cdm_source_info.csv", "analyses.csv",
      "arguments.csv"
    ),

    ## Fields ----
    .nResults = 0,
    .results = list(),

    ## Overrided ----
    .UI = function() {},
    .server = function(input, output, session) {},

    ## Methods ----
    loadResults = function(dots) {
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
    loadFromTPR = function(tpr) {
      browser()
      for (i in seq_len(length(private$.FILENAMES))) {
        file <- private$.FILENAMES[i]
        label <- substr(file, start = 1, stop = nchar(file) - 4)
        private$.results[[label]] <- dplyr::bind_rows(
          private$.results[[label]],
          tpr$attrition %>%
            dplyr::mutate(result_id = private$.nResults)
        )
      }
    },
    loadFromCSV = function(dir) {
      for (i in seq_len(length(private$.FILENAMES))) {
        file <- private$.FILENAMES[i]
        label <- substr(file, start = 1, stop = nchar(file) - 4)
        private$.results[[label]] <- dplyr::bind_rows(
          private$.results[[label]],
          read.csv(file.path(dir, file)) %>%
            dplyr::mutate(result_id = private$.nResults)
        )
      }
    },
    loadFromZIP = function(zipFile) {
      tempDir <- file.path(tempdir(), "tp-zip")
      unzip(zipFile, exdir = tempDir)
      private$loadFromCSV(tempDir)
    }
  )
)

# tpr <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/output.zip")
# tpr2 <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/output.zip")
#
# tprTp <- TreatmentPatterns$new(tpr, tpr2)
#
# tprTp$.__enclos_env__$private$.results$meta
