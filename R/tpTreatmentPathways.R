TPTreatmentPathways <- R6::R6Class(
  classname = "TPTreatmentPathways",
  inherit = ShinyModule,

  # Active ----
  active = list(),

  # Public ----
  public = list(
    initialize = function(db_summaryEventDuration, db_cdmSourceInfo) {
      super$initialize()

      private$.db_summaryEventDuration <- db_summaryEventDuration
      # private$.db_cdmSourceInfo <- db_cdmSourceInfo

      private$initPlot()
      private$initTable()
      private$initInputPanel()
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .db_summaryEventDuration = NULL,
    .db_cdmSourceInfo = NULL,

    ## Nested Modules ----
    .plot = NULL,
    .table = NULL,
    .inputPanel = NULL,

    ## Overrides ----
    .UI = function() {
      shiny::tagList(
        private$.inputPanel$UI(),
        private$.plot$UI(),
        private$.table$UI()
      )
    },
    .server = function(input, output, session) {
      private$.plot$server(input, output, session)
      private$.table$server(input, output, session)
      private$.inputPanel$server(input, output, session)

      dat <- private$.db_summaryEventDuration |>
        dplyr::collect()

      lines <- dat$line |>
        as.numeric() |>
        unique()

      lines <- lines[!is.na(lines)]

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(
          shiny::NS(private$.inputPanel$moduleName, private$.inputPanel$instanceId),
          "lines"
        ),
        choices = lines,
        selected = lines
      )

      dat$selected <- "0"

      private$.table$data <- dat |>
        dplyr::select(-"selected")

      private$.plot$args$eventDurations <- dat
      shiny::observeEvent(list(private$.table$bindings$rows_selected, private$.inputPanel$inputValues$lines),
        {
          dat$selected <- "0"
          dat[private$.table$bindings$rows_selected, "selected"] <- "1"
          private$.plot$args$eventDurations <- dat
          print(as.numeric(private$.inputPanel$inputValues$lines))
          private$.plot$args$eventLines <- as.numeric(private$.inputPanel$inputValues$lines)
        },
        ignoreNULL = TRUE
      )
    },

    ## Methods ----
    initPlot = function() {
      plotFun <- function(eventDurations, minCellCount = 1, treatmentGroups = "both", eventLines = c(1), includeOverall = TRUE) {
        print(eventLines)
        TreatmentPatterns::plotEventDuration(
          eventDurations = eventDurations,
          minCellCount = minCellCount,
          treatmentGroups = treatmentGroups,
          eventLines = eventLines,
          includeOverall = includeOverall
        ) +
          ggplot2::aes(fill = selected) +
          ggplot2::theme(legend.position = "none")
      }

      private$.plot <- PlotStatic$new(
        fun = plotFun,
        args = list(eventDurations = NULL),
        title = NULL
      )
      private$.plot$parentNamespace <- self$namespace
    },
    initTable = function() {
      private$.table <- Table$new(
        data = NULL,
        title = NULL,
        filter = "none"
      )
      private$.table$parentNamespace <- self$namespace
    },
    initInputPanel = function() {
      private$.inputPanel <- InputPanel$new(
        funs = list(
          lines = shinyWidgets::pickerInput
        ),
        args = list(
          lines = list(
            inputId = "lines",
            label = "Event Lines",
            choices = NULL,
            selected = "1",
            multiple = TRUE,
            list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
          )
        ),
        growDirection = "horizontal"
      )
      private$.inputPanel$parentNamespace <- self$namespace
    }
  )
)

TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
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
            .data$cdm_source_abbreviation %in% dupes ~ paste0(.data$cdm_source_abbreviation, " - Analysis: ", .data$analysis_id, " Result: ", .data$result_id),
            .default = .data$cdm_source_abbreviation
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

# tpr1 <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/")
# tpr2 <- TreatmentPatterns::TreatmentPatternsResults$new(filePath = "./inst/dummyData/TreatmentPatterns/3.0.0/")
# mod <- TreatmentPatterns$new(tpr1, tpr2)
#
# preview(mod)
#
# dir.exists(mod$.__enclos_env__$private$.dataInterface$.__enclos_env__$private$.DBDIR)
#
# preview(mod)
# file.exists(mod$.__enclos_env__$private$.dataInterface$.__enclos_env__$private$.DBDIR)
# mod$.__enclos_env__$private$.dataInterface$database$connect()
# DBI::dbListTables(mod$.__enclos_env__$private$.dataInterface$database$connection)
#
# preview(mod)
