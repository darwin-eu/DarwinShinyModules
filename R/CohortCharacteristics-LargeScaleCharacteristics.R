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

      private$initTable()
      private$initTopTable()
      private$initPlot()
      private$initComparePlot()
    }
  ),

  # Private ----
  private = list(
    .result = NULL,
    .tidyResult = NULL,

    # Nested modules
    .table = NULL,
    .topTable = NULL,
    .plot = NULL,
    .comparePlot = NULL,

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
            private$.plotCompareUI()
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
            inputId = shiny::NS(self$namespace, "tbl_cdm_name"),
            label = "CDM Name",
            choices = cdmNames,
            selected = cdmNames[1],
            multiple = TRUE,
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tbl_cohort_name"),
            label = "Cohort Name",
            choices = cohortNames,
            selected = cohortNames[1],
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tbl_compareBy"),
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
            inputId = shiny::NS(self$namespace, "tbl_smdReference"),
            label = "SMD reference",
            choices = c()
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "tbl_hide"),
            label = "Columns to hide",
            choices = CohortCharacteristics::availableTableColumns(private$.result),
            multiple = TRUE
          )
        ),
        shiny::column(width = 9, private$.table$UI())
      )
    },

    .tableTopUI = function() {
      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = shiny::NS(self$namespace, "top_n"),
          label = "Top Concepts",
          choices = c(5, 10, 25, 50, 100),
          selected = 5
        ),
        private$.topTable$UI()
      )
    },

    .plotUI = function() {
      shiny::tagList(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plot_facet"),
            label = "Facet",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "plot_colour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(width = 9, private$.plot$UI())
      )
    },

    .plotCompareUI = function() {
      shiny::tagList(
        shiny::column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "c_plot_colour"),
            label = "Colour",
            choices = c("cdm_name", "cohort_name", "variable_level", "type"),
            selected = "cdm_name",
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "c_plot_reference"),
            label = "Reference",
            choices = c(),
            multiple = FALSE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "c_plot_facet_x"),
            label = "Facet X",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          ),
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "c_plot_facet_y"),
            label = "Facet Y",
            choices = CohortCharacteristics::availablePlotColumns(private$.result),
            selected = NULL,
            multiple = TRUE
          )
        ),
        shiny::column(
          width = 9,
          private$.comparePlot$UI()
        )
      )
    },

    .server = function(input, output, session) {
      private$.serverUpdateComaprePlotPickers(input, output, session)
      private$.serverUpdateTablePickers(input, output, session)

      private$.serverTable(input, output, session)
      private$.serverTopTable(input, output, session)
      private$.serverPlot(input, output, session)
      private$.serverComparePlot(input, output, session)
    },

    .serverUpdateComaprePlotPickers = function(input, output, session) {
      shiny::observeEvent(input$c_plot_colour, {
        choices <- if (input$c_plot_colour == "cdm_name") {
          unique(result$cdm_name)
        } else if (input$c_plot_colour == "cohort_name") {
          result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::pull(.data$group_level) |>
            unique()
        } else if (input$c_plot_colour == "variable_level") {
          result |>
            dplyr::pull(.data$variable_level) |>
            unique()
        } else if (input$c_plot_colour == "type") {
          settings <- omopgenerics::settings(result)
          settings$type |>
            unique()
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "c_plot_reference",
          choices = choices
        )
      })
    },

    .serverUpdateTablePickers = function(input, output, session) {
      shiny::observeEvent(list(input$tbl_compareBy, input$tbl_cdm_name, input$tbl_cohort_name), {
        strata <- result |>
          dplyr::distinct(.data$strata_name) |>
          dplyr::pull(.data$strata_name)

        choices <- if (input$tbl_compareBy == "cdm_name") {
          cdmNames <- private$.result |>
            dplyr::distinct(.data$cdm_name) |>
            dplyr::pull(.data$cdm_name)
          cdmNames[cdmNames %in% input$tbl_cdm_name]
        } else if (input$tbl_compareBy == "cohort_name") {
          cohortNames <- private$.result |>
            dplyr::filter(.data$group_name == "cohort_name") |>
            dplyr::distinct(.data$group_level) |>
            dplyr::pull(.data$group_level)
          cohortNames[cohortNames %in% input$tbl_cohort_name]
        } else if (input$tbl_compareBy == "variable_level") {
          private$.result |>
            dplyr::distinct(.data$variable_level) |>
            dplyr::pull(.data$variable_level)
        } else if (input$tbl_compareBy == "type") {
          settings <- omopgenerics::settings(private$.result)
          settings$type |>
            unique()
        } else if (input$tbl_compareBy %in% strata) {
          private$.result |>
            dplyr::filter(.data$strata_name == input$tbl_compareBy) |>
            dplyr::distinct(.data$strata_level) |>
            dplyr::pull(.data$strata_level)
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "tbl_smdReference",
          choices = choices
        )
      })
    },

    .serverTable = function(input, output, session) {
      shiny::observeEvent(list(
        input$tbl_compareBy,
        input$tbl_hide,
        input$tbl_smdReference,
        input$tbl_cdm_name,
        input$tbl_cohort_name
      ), {
        private$.table$args$compareBy <- input$tbl_compareBy
        private$.table$args$hide <- input$tbl_hide
        private$.table$args$smdReference <- input$tbl_smdReference

        private$.table$args$result <- private$.result |>
          dplyr::filter(
            .data$cdm_name %in% input$tbl_cdm_name,
            .data$group_level %in% input$tbl_cohort_name
          )

        private$.table$server(input, output, session)
      })
    },

    .serverTopTable = function(input, output, session) {
      shiny::observeEvent(input$top_n, {
        private$.topTable$args$topConcepts <- input$top_n
        private$.topTable$server(input, output, session)
      })
    },

    .serverPlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$plot_facet,
        input$plot_colour
      ), {
        private$.plot$args$facet <- input$plot_facet
        private$.plot$args$colour <- input$plot_colour
        private$.plot$server(input, output, session)
      })
    },

    .serverComparePlot = function(input, output, session) {
      shiny::observeEvent(list(
        input$c_plot_colour,
        input$c_plot_reference,
        input$c_plot_facet_x,
        input$c_plot_facet_y
      ), {
        private$.comparePlot$args$colour <- input$c_plot_colour
        private$.comparePlot$args$reference <- input$c_plot_reference

        f <- sprintf(
          "%s ~ %s",
          paste(input$c_plot_facet_y, collapse = " + "),
          paste(input$c_plot_facet_x, collapse = " + ")
        )

        private$.comparePlot$args$facet <- if (f == " ~ ") {
          NULL
        } else if (stringr::str_detect(string = f, pattern = "^ \\~")) {
          as.formula(paste0(".", f))
        } else if (stringr::str_detect(string = f, pattern = "\\~ $")) {
          as.formula(paste0(f, "."))
        } else {
          as.formula(f)
        }

        facets <- unique(c(input$c_plot_facet_x, input$c_plot_facet_y))

        if (is.null(facets)) {
          private$.comparePlot$args$result <- private$.result |>
            dplyr::filter(.data$strata_name == "overall")
        } else {
          private$.comparePlot$args$result <- private$.result |>
            dplyr::filter(.data$strata_name %in% c("overall", facets))
        }

        private$.comparePlot$server(input, output, session)
      })
    },

    checkNone = function(val) {
      if (val == "none") {
        NULL
      } else {
        val
      }
    },
    initTable = function() {
      private$.table <- DarwinShinyModules::DTTable$new(
        fun = CohortCharacteristics::tableLargeScaleCharacteristics,
        args = list(result = private$.result, type = "DT"),
        parentNamespace = self$namespace
      )
    },

    initTopTable = function() {
      private$.topTable <- GTTable$new(
        fun = CohortCharacteristics::tableTopLargeScaleCharacteristics,
        args = list(result = private$.result),
        parentNamespace = self$namespace
      )
    },
    initPlot = function() {
      private$.plot <- PlotPlotly$new(
        fun = CohortCharacteristics::plotLargeScaleCharacteristics,
        args = list(result = private$.result),
        parentNamespace = self$namespace
      )
    },
    initComparePlot = function() {
      plotFun <- function(...) {
        CohortCharacteristics::plotComparedLargeScaleCharacteristics(...) +
          ggplot2::coord_fixed(ratio = 1 / 1)
      }

      private$.comparePlot <- PlotPlotly$new(
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
