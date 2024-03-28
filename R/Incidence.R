Incidence <- R6::R6Class(
  classname = "Incidence",
  inherit = Table,

  # Public ----
  public = list(
    UI = function(title = "Table") {
      shiny::tagList(
        shiny::h3(title),
        shiny::div(
          style = "display: inline-block; vertical-align: top; width: 150px",
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("filterSex")))
        ),
        shiny::div(
          style = "display: inline-block; vertical-align: top; width: 150px",
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("filterAge")))
        ),
        shiny::div(
          style = "display: inline-block; vertical-align: top; width: 150px",
          shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("filterYear")))
        ),
        DT::DTOutput(outputId = shiny::NS(private$.appId, private$id("table"))),
        shiny::downloadButton(outputId = shiny::NS(private$.appId, private$id("dlButton")), label = "csv")
      )
    },

    server = function(input, output, session) {
      private$readData()
      private$renderFilters(output)
      private$renderTable(input, output)
      private$downloader(output)
    }
  ),

  # Private ----
  private = list(
    ## Methods ----
    filter = function(input) {
      shiny::req(input[[private$id("sex")]])
      shiny::req(input[[private$id("age")]])
      shiny::req(input[[private$id("year")]])
      private$.data %>%
        dplyr::filter(
          .data$denominator_sex %in% input[[private$id("sex")]],
          .data$denominator_age_group %in% input[[private$id("age")]],
          lubridate::year(.data$incidence_start_date) %in% input[[private$id("year")]]
        )
    },

    renderFilters = function(output) {
      output[[private$id("filterSex")]] <- shiny::renderUI({
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          shinyWidgets::pickerInput(
            label = "Sex",
            inputId = shiny::NS(private$.appId, private$id("sex")),
            choices = unique(private$.data$denominator_sex),
            selected = unique(private$.data$denominator_sex),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            )
          )
        )
      })

      output[[private$id("filterAge")]] <- shiny::renderUI({
        shinyWidgets::pickerInput(
          label = "Age Group",
          inputId = shiny::NS(private$.appId, private$id("age")),
          choices = unique(private$.data$denominator_age_group),
          selected = unique(private$.data$denominator_age_group),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          )
        )
      })

      output[[private$id("filterYear")]] <- shiny::renderUI({
        shinyWidgets::pickerInput(
          label = "Year",
          inputId = shiny::NS(private$.appId, private$id("year")),
          choices = unique(lubridate::year(private$.data$incidence_start_date)),
          selected = unique(lubridate::year(private$.data$incidence_start_date)),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          )
        )
      })

      # output[[private$id("filterYear")]] <- shiny::renderUI({
      #   shiny::dateRangeInput(
      #     inputId = shiny::NS(private$.appId, private$id("year")),
      #     label = "Start Date",
      #     start = as.Date(min(private$.data$incidence_start_date)),
      #     end = as.Date(max(private$.data$incidence_start_date))
      #   )
      # })
    },

    renderTable = function(input, output) {
      output[[private$id("table")]] <- DT::renderDT(
        expr = private$filter(input),
        options = list(scrollX = TRUE)
      )
    },

    readData = function() {
      private$.data <- if (endsWith(x = tolower(private$.dataPath), suffix = ".csv")) {
        private$fromCSV()
      } else if (endsWith(x = tolower(private$.dataPath), suffix = ".rds")) {
        private$fromRDS()
      }
    },

    fromCSV = function() {
      inc <- super$readData()[-1]
      class(inc) <- c("IncidencePrevalenceResult", "IncidenceResult", class(inc))

      return(inc)
    },

    fromRDS = function() {
      return(readRDS(private$.dataPath))
    }
  ),

  # Active ----
  active = list()
)
