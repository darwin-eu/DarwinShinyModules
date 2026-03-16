LargeScaleCharacteristics <- R6::R6Class(
  classname = "LargeScaleCharacteristics",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    result = function() {
      return(private$.result)
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)
      if ("summarised_result" %in% class(result)) {
        private$.result <- result
        private$.tidyResult <- omopgenerics::tidy(result)
      } else {
        stop("Data has to be of class: `summarised_result`")
      }

      private$.initTable()
      private$.initTableTop()
      private$.initPlot()
      private$.initPlotCompared()
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,
    .tidyResult = NULL,

    # Nested modules
    .table = NULL,
    .tableTop = NULL,
    .plot = NULL,
    .plotCompared = NULL,

    ## UI ----
    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Large Scale Characteristics",
            private$.tableUI()
          ),
          shiny::tabPanel(
            title = "Top Concepts",
            private$.tableTopUI()
          ),
          shiny::tabPanel(
            title = "Plot",
            private$.plotUI()
          ),
          shiny::tabPanel(
            title = "Compare Plot",
            private$.plotComparedUI()
          )
        )
      )
    },

    .tableUI = function() {
      cdmNames <- private$.result |>
        dplyr::distinct(.data$cdm_name) |>
        dplyr::pull(.data$cdm_name)

      cohortNames <- private$.result |>
        dplyr::filter(.data$group_name == "cohort_name") |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)

      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCDMName"),
            label = "CDM Name",
            choices = cdmNames,
            selected = cdmNames[1],
            multiple = TRUE,
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCohortName"),
            label = "Cohort Name",
            choices = cohortNames,
            selected = cohortNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableCompareBy"),
            label = "Compare By",
            choices = c(
              "cdm_name", "cohort_name", "variable_level", "type",
              private$.result |>
                dplyr::pull(.data$strata_name) |>
                unique()
            ),
            selected = "cdm_name"
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableSMDReference"),
            label = "SMD reference",
            choices = c()
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableHide"),
            label = "Columns to hide",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            multiple = TRUE
          )
        ),
        shiny::column(width = 9, private$.table$UI())
      )
    },

    .tableTopUI = function() {
      cdmNames <- private$.result |>
        dplyr::distinct(.data$cdm_name) |>
        dplyr::pull(.data$cdm_name)

      cohortNames <- private$.result |>
        dplyr::filter(.data$group_name == "cohort_name") |>
        dplyr::distinct(.data$group_level) |>
        dplyr::pull(.data$group_level)

      strata <- private$.result |>
        dplyr::distinct(.data$strata_name) |>
        dplyr::pull(.data$strata_name)

      windows <- private$.result |>
        dplyr::distinct(.data$variable_level) |>
        dplyr::pull(.data$variable_level)

      shiny::tagList(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopCDMName"),
            label = "CDM Name",
            choices = cdmNames,
            selected = cdmNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopCohortName"),
            label = "Cohort Name",
            choices = cohortNames,
            selected = cohortNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopStrata"),
            label = "Strata",
            choices = strata,
            selected = strata[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopVariableLevel"),
            label = "Window",
            choices = windows,
            selected = windows[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tableTopN"),
            label = "Top Concepts",
            choices = c(5, 10, 25, 50, 100),
            selected = 5
          )
        ),
        private$.tableTop$UI()
      )
    },

    .plotUI = function() {
      shiny::tagList(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotFacet"),
            label = "Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotColour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(width = 9, private$.plot$UI())
      )
    },

    .plotComparedUI = function() {
      shiny::tagList(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedColour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = "cdm_name",
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedReference"),
            label = "Reference",
            choices = c(),
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedFacetX"),
            label = "Horizontal Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plotComparedFacetY"),
            label = "Vertical Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 9,
          private$.plotCompared$UI()
        )
      )
    },

    ## Server ----
    .server = function(input, output, session) {
      private$.updateTable(input, output, session)
      private$.updatePlotCompared(input, output, session)

      private$.serverTable(input, output, session)
      private$.serverTopTable(input, output, session)
      private$.serverPlot(input, output, session)
      private$.serverComparePlot(input, output, session)
    },

    .updateTable = function(input, output, session) {
      shiny::observeEvent(list(input$tableCompareBy, input$tableCDMName, input$tableCohortName), {
        strata <- result |>
          dplyr::distinct(.data$strata_name) |>
          dplyr::pull(.data$strata_name)

        choices <- if (input$tableCompareBy == "cdm_name") {
          cdmNames <- private$.result |>
            dplyr::distinct(.data$cdm_name) |>
            dplyr::pull(.data$cdm_name)
          cdmNames[cdmNames %in% input$tableCDMName]
        } else if (input$tableCompareBy == "cohort_name") {
          cohortNames <- private$.result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::distinct(.data$group_level) |>
            dplyr::pull(.data$group_level)
          cohortNames[cohortNames %in% input$tableCohortName]
        } else if (input$tableCompareBy == "variable_level") {
          private$.result |>
            dplyr::distinct(.data$variable_level) |>
            dplyr::pull(.data$variable_level)
        } else if (input$tableCompareBy == "type") {
          settings <- omopgenerics::settings(private$.result)
          settings$type |>
            unique()
        } else if (input$tableCompareBy %in% strata) {
          private$.result |>
            dplyr::filter(.data$strata_name == input$tableCompareBy) |>
            dplyr::distinct(.data$strata_level) |>
            dplyr::pull(.data$strata_level)
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "tableSMDReference",
          choices = choices
        )
      })
    },

    .updatePlotCompared = function(input, output, session) {
      shiny::observeEvent(input$plotComparedColour, {
        choices <- if (input$plotComparedColour == "cdm_name") {
          unique(result$cdm_name)
        } else if (input$plotComparedColour == "cohort_name") {
          result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::pull(.data$group_level) |>
            unique()
        } else if (input$plotComparedColour == "variable_level") {
          result |>
            dplyr::pull(.data$variable_level) |>
            unique()
        } else if (input$plotComparedColour == "type") {
          settings <- omopgenerics::settings(result)
          settings$type |>
            unique()
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "plotComparedReference",
          choices = choices
        )
      })
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$tableCompareBy,
        input$tableHide,
        input$tableSMDReference,
        input$tableCDMName,
        input$tableCohortName
      ), {
        private$.table$args$compareBy <- input$tableCompareBy
        private$.table$args$hide <- input$tableHide
        private$.table$args$smdReference <- input$tableSMDReference

        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$tableCDMName,
            .data$group_level %in% input$tableCohortName
          )

        private$.table$server(input, output, session)
      })
    },

    .serverTopTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$tableTopN,
        input$tableTopCDMName,
        input$tableTopCohortName,
        input$tableTopStrata,
        input$tableTopVariableLevel
      ), {
        private$.tableTop$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$tableTopCDMName,
            .data$strata_name %in% input$tableTopStrata,
            .data$variable_level %in% input$tableTopVariableLevel
          ) |>
          omopgenerics::filterGroup(.data$cohort_name %in% input$tableTopCohortName)

        private$.tableTop$args$topConcepts <- input$tableTopN
        private$.tableTop$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$plotFacet,
        input$plotColour
      ), {
        private$.plot$args$facet <- input$plotFacet
        private$.plot$args$colour <- input$plotColour
        private$.plot$server(input, output, session)
      })
    },

    .serverComparePlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$plotComparedColour,
        input$plotComparedReference,
        input$plotComparedFacetX,
        input$plotComparedFacetY
      ), {
        private$.plotCompared$args$colour <- input$plotComparedColour
        private$.plotCompared$args$reference <- input$plotComparedReference

        f <- sprintf(
          "%s ~ %s",
          paste(input$plotComparedFacetY, collapse = " + "),
          paste(input$plotComparedFacetX, collapse = " + ")
        )

        private$.plotCompared$args$facet <- if (f == " ~ ") {
          NULL
        } else if (stringr::str_detect(string = f, pattern = "^ \\~")) {
          as.formula(paste0(".", f))
        } else if (stringr::str_detect(string = f, pattern = "\\~ $")) {
          as.formula(paste0(f, "."))
        } else {
          as.formula(f)
        }

        facets <- unique(c(input$plotComparedFacetX, input$plotComparedFacetY))

        if (is.null(facets)) {
          private$.plotCompared$args$result <- private$.result |>
            dplyr::filter(.data$strata_name == "overall")
        } else {
          private$.plotCompared$args$result <- private$.result |>
            dplyr::filter(.data$strata_name %in% c("overall", facets))
        }

        private$.plotCompared$server(input, output, session)
      })
    },

    ## Init ----
    .initTable = function() {
      private$.table <- DarwinShinyModules::DTTable$new(
        fun = CohortCharacteristics::tableLargeScaleCharacteristics,
        args = list(result = private$.result, type = "DT"),
        parentNamespace = self$namespace
      )
    },

    .initTableTop = function() {
      private$.tableTop <- DarwinShinyModules::Flextable$new(
        fun = CohortCharacteristics::tableTopLargeScaleCharacteristics,
        args = list(result = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
    },

    .initPlot = function() {
      private$.plot <- DarwinShinyModules::PlotPlotly$new(
        fun = CohortCharacteristics::plotLargeScaleCharacteristics,
        args = list(result = private$.result, style = "darwin"),
        parentNamespace = self$namespace
      )
    },

    .initPlotCompared = function() {
      plotFun <- function(...) {
        CohortCharacteristics::plotComparedLargeScaleCharacteristics(...) +
          ggplot2::coord_fixed(ratio = 1 / 1)
      }

      private$.plotCompared <- DarwinShinyModules::PlotPlotly$new(
        fun = plotFun,
        args = list(result = private$.result, style = "darwin"),
        width = "108vh",
        height = "90vh",
        inline = TRUE,
        title = NULL,
        parentNamespace = self$namespace
      )
    }
  )
)
