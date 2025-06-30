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
