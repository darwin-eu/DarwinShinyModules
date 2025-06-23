TreatmentPatternsMod <- R6::R6Class(
  classname = "TreatmentPatternsMod",
  inherit = ShinyModule,

  # Active ----
  active = list(),

  # Publc ----
  public = list(
    initialize = function(...) {
      super$initialize()
      dots <- list(...)
      private$loadResults(dots)
      private$setColours()
      private$initSankey()
      private$initSunburst()
      private$initInputPanel()
      private$initTable()
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
    .colours = NULL,

    ## Sub Modules ----
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
      private$.inputPanel$server(input, output, session)
      private$.sunburst$server(input, output, session)
      private$.sankey$server(input, output, session)
      private$.table$server(input, output, session)

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
          dat <- private$.results$cdm_source_info |>
            dplyr::inner_join(
              private$.results$treatment_pathways,
              by = dplyr::join_by(
                result_id == result_id,
                analysis_id == analysis_id
              )
            ) |>
            dplyr::filter(
              .data$cdm_source_abbreviation == private$.inputPanel$inputValues$database
            )

          resultId <- dat |>
            dplyr::pull(.data$result_id) |>
            unique()

          analysisId <- dat |>
            dplyr::pull(.data$analysis_id) |>
            unique()

          filteredData <- private$.results$treatment_pathways |>
            dplyr::filter(
              .data$age == private$.inputPanel$inputValues$age,
              .data$sex == private$.inputPanel$inputValues$sex,
              .data$index_year == private$.inputPanel$inputValues$indexYear,
              .data$analysis_id == analysisId,
              .data$result_id == resultId,
              .data$freq >= private$.inputPanel$inputValues$minFreq,
            )

          private$.sunburst$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations
          private$.sankey$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations

          private$.sunburst$args$treatmentPathways <- filteredData
          private$.sankey$args$treatmentPathways <- filteredData
          private$.table$reactiveValues$data <- filteredData
        }
      )
    },

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
      for (i in seq_len(length(private$.FILENAMES))) {
        file <- private$.FILENAMES[i]
        label <- substr(file, start = 1, stop = nchar(file) - 4)
        private$.results[[label]] <- dplyr::bind_rows(
          private$.results[[label]],
          tpr[[label]] |>
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
          read.csv(file.path(dir, file)) |>
            dplyr::mutate(result_id = private$.nResults)
        )
      }
    },
    loadFromZIP = function(zipFile) {
      tempDir <- file.path(tempdir(), "tp-zip")
      unzip(zipFile, exdir = tempDir)
      private$loadFromCSV(tempDir)
    },
    initInputPanel = function() {
      databaseLabels <- private$.results$cdm_source_info |>
        dplyr::inner_join(
          private$.results$treatment_pathways,
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
            choices = unique(private$.results$treatment_pathways$age),
            selected = "all",
            multiple = FALSE
          ),
          sex = list(
            inputId = shiny::NS(self$namespace, "sex"),
            label = "Sex",
            choices = unique(private$.results$treatment_pathways$sex),
            selected = "all",
            multiple = FALSE
          ),
          indexYear = list(
            inputId = shiny::NS(self$namespace, "indexYear"),
            label = "Index Year",
            choices = unique(private$.results$treatment_pathways$index_year),
            selected = "all",
            multiple = FALSE
          ),
          minFreq = list(
            inputId = shiny::NS(self$namespace, "minFreq"),
            label = "Minimum Frequency",
            min = min(private$.results$treatment_pathways$freq),
            max = max(private$.results$treatment_pathways$freq),
            value = min(private$.results$treatment_pathways$freq)
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
        nodes <- private$.results$treatment_pathways$pathway |>
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



tpr <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/output.zip")
# tpr2 <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/output.zip")

mod1 <- TreatmentPatternsMod$new(tpr)
# mod2 <- TreatmentPatternsMod$new(tpr2)

# mod1$instanceId
# mod2$instanceId
#
# mod1$namespace
# mod2$namespace

DarwinShinyModules::launchDarwinDashboardApp(
  list(TreatmentPatterns = list(mod1))
)

# tprTp$.__enclos_env__$private$.results$meta
