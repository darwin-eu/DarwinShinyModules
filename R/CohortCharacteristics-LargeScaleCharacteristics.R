LargeScaleCharacteristics <- R6::R6Class(
  classname = "LargeScaleCharacteristics",
  inherit = DarwinShinyModules::ShinyModule,
  active = list(
    data = function() {
      return(private$.data)
    },
    table = function() {
      return(private$.table)
    }
  ),
  public = list(
    initialize = function(data = NULL) {
      super$initialize()
      private$.data <- omopgenerics::tidy(data)

      private$.table <- DarwinShinyModules::Table$new(
        data = private$.data,
        title = NULL
      )
      private$.table$parentNamespace <- self$namespace

      x <- CohortCharacteristics::tableLargeScaleCharacteristics(
        result = data,
        compareBy = "cohort_name",
        smdReference = "benzodiazepines",
        type = "DT"
      )

      private$.tidyTable <- DarwinShinyModules::Table$new(
        data = x$x$data,
        title = NULL,
        options = list(scrollX = TRUE),
        filter = "top"
      )
      private$.tidyTable$parentNamespace <- self$namespace
    }
  ),
  private = list(
    .data = NULL,
    .tidyTable = NULL,
    .table = NULL,
    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Raw",
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "rows"),
                label = "Rows",
                choices = c(10, 25, 50, Inf),
                selected = 10,
                multiple = FALSE
              )
            ),
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "database"),
                label = "Database",
                choices = unique(private$.data$cdm_name),
                selected = unique(private$.data$cdm_name)[1],
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
              )
            ),
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "cohort"),
                label = "Cohort",
                choices = unique(private$.data$cohort_name),
                selected = unique(private$.data$cohort_name)[1],
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
              )
            ),
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "variable_level"),
                label = "Time Window",
                choices = unique(private$.data$variable_level),
                selected = unique(private$.data$variable_level)[1],
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
              )
            ),
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "age_group"),
                label = "Age Group",
                choices = unique(private$.data$age_group),
                selected = "overall",
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
              )
            ),
            shiny::div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              shinyWidgets::pickerInput(
                inputId = shiny::NS(self$namespace, "pregnancy_period"),
                label = "Preganncy Period",
                choices = unique(private$.data$pregnancy_period),
                selected = "overall",
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
              )
            ),
            private$.table$UI()
          ),
          shiny::tabPanel(
            title = "Tidy",
            private$.tidyTable$UI()
          )
        )
      )
    },
    .server = function(input, output, session) {
      shiny::observeEvent(
        list(
          input$rows,
          input$cohort,
          input$database,
          input$age_group,
          input$pregnancy_period,
          input$variable_level
        ),
        handlerExpr = {
          if (!is.null(private$.data$count)) {
            private$.table$data <- private$.data %>%
              dplyr::group_by(.data$cdm_name) %>%
              dplyr::filter(.data$cohort_name %in% input$cohort) %>%
              dplyr::filter(.data$cdm_name %in% input$database) %>%
              dplyr::filter(.data$age_group %in% input$age_group) %>%
              dplyr::filter(.data$pregnancy_period %in% input$pregnancy_period) %>%
              dplyr::filter(.data$variable_level %in% input$variable_level) %>%
              dplyr::arrange(dplyr::desc(.data$count)) %>%
              head(n = as.numeric(input$rows))
          }
        }
      )



      # shiny::observeEvent(input$cohort, handlerExpr = {
      #   private$.table$data <- private$.data %>%
      #     dplyr::filter(.data$cohort_name %in% input$cohort)
      # })
      #
      # shiny::observeEvent(input$database, handlerExpr = {
      #   private$.table$data <- private$.data %>%
      #     dplyr::filter(.data$cdm_name %in% input$database)
      # })
      #
      # shiny::observeEvent(input$age_group, handlerExpr = {
      #   private$.table$data <- private$.data %>%
      #     dplyr::filter(.data$age_group %in% input$age_group)
      # })
      #
      # shiny::observeEvent(input$pregnancy_period, handlerExpr = {
      #   private$.table$data <- private$.data %>%
      #     dplyr::filter(.data$pregnancy_period %in% input$pregnancy_period)
      # })
      #
      # shiny::observeEvent(input$variable_level, handlerExpr = {
      #   private$.table$data <- private$.data %>%
      #     dplyr::filter(.data$variable_level %in% input$variable_level)
      # })

      private$.table$server(input, output, session)
      private$.tidyTable$server(input, output, session)
    }
  )
)
