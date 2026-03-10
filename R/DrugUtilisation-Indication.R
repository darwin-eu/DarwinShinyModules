Indication <- R6::R6Class(
  classname = "Indication",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    data = function() {
      return(private$.data)
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)
      private$.result <- result

      private$.plot <- DarwinShinyModules::PlotPlotly$new(
        fun = DrugUtilisation::plotIndication,
        args = list(result = private$.result, style = "darwin"),
        title = NULL,
        parentNamespace = self$namespace
      )

      private$.table <- DarwinShinyModules::Flextable$new(
        fun = DrugUtilisation::tableIndication,
        args = list(result = private$.result, type = "flextable", style = "darwin"),
        parentNamespace = self$namespace
      )
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,
    .table = NULL,
    .plot = NULL,
    .inputPanelTable = NULL,
    .inputPanelPlot = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::column(
          width = 2,
          shinyWidgets::pickerInput(
            inputId = shiny::NS(self$namespace, "cdm_name"),
            label = "CDM Name",
            choices = unique(private$.result$cdm_name),
            selected = unique(private$.result$cdm_name)[1],
            multiple = TRUE
          ),
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Table Settings",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "header"),
                label = "Header",
                choices = c("cdm_name", "cohort_name", DrugUtilisation::strataColumns(private$.result)),
                selected = c("cdm_name", "cohort_name", DrugUtilisation::strataColumns(private$.result)),
                multiple = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "groupColumn"),
                label = "Group Column",
                choices = "variable_name",
                selected = "variable_name",
                multiple = TRUE
              ),
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "hide"),
                label = "Columns to hide",
                choices = c(
                  "window_name", "mutually_exclusive", "unknown_indication_table",
                  "censor_date", "cohort_table_name", "index_date",
                  "indication_cohort_name"
                ),
                selected = c(
                  "window_name", "mutually_exclusive", "unknown_indication_table",
                  "censor_date", "cohort_table_name", "index_date",
                  "indication_cohort_name"
                ),
                multiple = TRUE
              )
            ),

            shiny::tabPanel(
              title = "Plot Settings",
              shinyWidgets::pickerInput(
                inputId = "x_axis",
                label = "X-Axis",
                choices = "variable_level",
                selected = "variable_level"
              ),
              shinyWidgets::pickerInput(
                inputId = "position",
                label = "Bar position",
                choices = c("stack", "dodge"),
                selected = "stack"
              ),
              shinyWidgets::pickerInput(
                inputId = "colour",
                label = "Colour",
                choices = private$.getSelectableColumns(),
                selected = private$.getSelectableColumns()[1]
              )
            )
          )
        ),
        shiny::column(
          width = 10,
          private$.plot$UI(),
          private$.table$UI(),
        )
      )
    },

    .server = function(input, output, session) {
      shiny::observeEvent(list(
        input$cdm_name,
        input$x,
        input$position,
        input$facet,
        input$colour
      ), {
        private$.plot$args$result <- private$.result |>
          dplyr::filter(.data$cdm_name %in% input$cdm_name)

        private$.plot$args$x <- private$.inputPanelPlot$InputValues$x
        private$.plot$args$position <- private$.inputPanelPlot$InputValues$position
        private$.plot$args$facet <- private$.inputPanelPlot$InputValues$facet
        private$.plot$args$colour <- private$.inputPanelPlot$InputValues$colour
        private$.plot$server(input, output, session)
      })

      shiny::observeEvent(list(
        input$cdm_name,
        input$header,
        input$groupColumn,
        input$hide
      ), {
        private$.table$args$result <- private$.result |>
          dplyr::filter(.data$cdm_name %in% input$cdm_name)

        private$.table$args$header <- input$header
        private$.table$args$groupColumn <- input$groupColumn
        private$.table$args$hide <- input$hide
        private$.table$server(input, output, session)
      })
    },

    .getSelectableColumns = function() {
      c(DrugUtilisation::groupColumns(private$.result), DrugUtilisation::strataColumns(private$.result))
    }
  )
)
