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

      private$.database <- DarwinShinyModules::DatabaseDBI$new(
        driver = duckdb::duckdb(dbdir = private$.DBDIR)
      )
      private$.database$parentNamespace <- self$namespace

      dots <- list(...)
      private$loadResults(dots)
    }
  ),

  # Private ----
  private = list(
    ## Constants ----
    .DBDIR = file.path(tempdir(), "tp_mod"),
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
      unlink(private$.DBDIR, recursive = TRUE, force = TRUE)
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
      on.exit(private$.database$disconnect())
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
        dplyr::copy_to(dest = private$.database$connection, df = data, name = label, overwrite = TRUE)
        private$.tables[[label]] <- dplyr::tbl(private$.database$connection, label)
      } else {
        private$.tables[[label]] <- private$.tables[[label]] |>
          dplyr::union_all(data, copy = TRUE) |>
          dplyr::compute(name = label, temporary = FALSE)
      }
    }
  )
)

TreatmentPatternsMod <- R6::R6Class(
  classname = "TreatmentPatternsMod",
  inherit = ShinyModule,

  # Active ----
  active = list(),

  # Public ----
  public = list(
    initialize = function(...) {
      super$initialize()
      private$.dataInterface <- TPDataInterface$new(...)
      private$.dataInterface$parentNamespace <- self$namespace

      private$.dataInterface$database$connect()
      on.exit(private$.dataInterface$database$disconnect())

      private$setColours()
      private$initSankey()
      private$initSunburst()
      private$initInputPanel()
      private$initTable()
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .colours = NULL,

    ## Nested Modules ----
    .dataInterface = NULL,
    .sunburst = NULL,
    .sankey = NULL,
    .inputPanel = NULL,
    .table = NULL,

    ## Overrided ----
    .UI = function() {
      shiny::tagList(
        shiny::column(
          width = 4,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 8,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Sunburst",
              private$.sunburst$UI()
            ),
            shiny::tabPanel(
              title = "Sankey",
              private$.sankey$UI()
            )
          )
        ),
        private$.table$UI()
      )
    },
    .server = function(input, output, session) {
      private$.dataInterface$server(input, output, session)
      private$.inputPanel$server(input, output, session)
      private$.sunburst$server(input, output, session)
      private$.sankey$server(input, output, session)
      private$.table$server(input, output, session)

      dupes <- private$.dataInterface$cdm_source_info |>
        dplyr::group_by(.data$cdm_source_abbreviation) |>
        dplyr::summarise(n = n()) |>
        dplyr::filter(.data$n > 1) |>
        dplyr::pull(.data$cdm_source_abbreviation)

      cdm_source_info <- private$.dataInterface$cdm_source_info |>
        dplyr::group_by(.data$cdm_source_abbreviation) |>
        dplyr::mutate(
          cdm_source_abbreviation = case_when(
            .data$cdm_source_abbreviation %in% dupes ~ paste0(.data$cdm_source_abbreviation, " - Analysis: ", .data$analysis_id, " Result: ", .data$result_id)
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::collect()

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(shiny::NS(private$.inputPanel$moduleName, private$.inputPanel$instanceId), "database"),
        choices = cdm_source_info$cdm_source_abbreviation
      )

      shiny::observeEvent(
        list(
          private$.inputPanel$inputValues$database,
          private$.inputPanel$inputValues$age,
          private$.inputPanel$inputValues$sex,
          private$.inputPanel$inputValues$indexYear,
          private$.inputPanel$inputValues$minFreq,
          private$.inputPanel$inputValues$groupCombinations
        ),
        {
          dat <- cdm_source_info |>
            dplyr::inner_join(
              private$.dataInterface$treatment_pathways,
              by = dplyr::join_by(
                result_id == result_id,
                analysis_id == analysis_id
              ), copy = TRUE
            ) |>
            dplyr::filter(
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

          private$.sunburst$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations
          private$.sankey$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations

          private$.sunburst$args$treatmentPathways <- dat
          private$.sankey$args$treatmentPathways <- dat
          private$.table$reactiveValues$data <- dat
        }
      )
    },

    ## Methods ----
    initInputPanel = function() {
      databaseLabels <- private$.dataInterface$cdm_source_info |>
        dplyr::inner_join(
          private$.dataInterface$treatment_pathways,
          by = dplyr::join_by(
            result_id == result_id,
            analysis_id == analysis_id
          )
        ) |>
        dplyr::pull(.data$cdm_source_abbreviation) |>
        unique()

      private$.inputPanel <- InputPanel$new(
        funs = list(
          database = shinyWidgets::pickerInput,
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
          age = list(
            inputId = shiny::NS(self$namespace, "age"),
            label = "Age",
            choices = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$age) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          sex = list(
            inputId = shiny::NS(self$namespace, "sex"),
            label = "Sex",
            choices = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$sex) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          indexYear = list(
            inputId = shiny::NS(self$namespace, "indexYear"),
            label = "Index Year",
            choices = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$index_year) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          minFreq = list(
            inputId = shiny::NS(self$namespace, "minFreq"),
            label = "Minimum Frequency",
            min = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$freq) |>
              min(),
            max = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$freq) |>
              max(),
            value = private$.dataInterface$treatment_pathways |>
              dplyr::pull(.data$freq) |>
              min()
          ),
          groupCombinations = list(
            inputId = shiny::NS(self$namespace, "groupCombinations"),
            label = "Group Combinations",
            value = FALSE
          )
        ),
        growDirection = "vertical"
      )
      private$.inputPanel$parentNamespace <- self$namespace
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
      if (is.null(private$.colours)) {
        nodes <- private$.dataInterface$treatment_pathways |>
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
      }
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
        title = NULL
      )
      private$.sankey$parentNamespace <- self$namespace
    },
    initSunburst = function() {
      # Drop "Stopped"
      # colours <- private$.colours[-length(private$.colours)]
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
        title = NULL
      )
      private$.sunburst$parentNamespace <- self$namespace
    },
    initTable = function() {
      private$.table <- Table$new(data = NULL, title = NULL, filter = "none")
      private$.table$parentNamespace <- self$namespace
    }
  )
)
