#' @title Table
#'
#' @include ShinyModule.R
#'
#' @description
#' Table Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' table <- Table$new(data = mtcars)
#'
#' if (interactive()) {
#'   preview(table)
#' }
Table <- R6::R6Class(
  classname = "Table",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field bindings (`reactivevalues`) Bindings of the DataTable in a reactive environment.
    data = function(data) {
      if (missing(data)) {
        return(isolate(private$.reactiveValues$data))
      } else {
        checkmate::assertDataFrame(data)
        private$.reactiveValues$data <- data
      }
    },

    title = function(title) {
      if (missing(title)) {
        return(private$.title)
      } else {
        checkmate::assertCharacter(title, len = 1)
        private$.title <- title
      }
    },

    #' @field data The data to render in the DataTable, usually a `data.frame`-like object.
    bindings = function() {
      return(private$.bindings)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param data (`data.frame`) Data to plot with, usually a `data.frame`-like object.
    #' @param options (`list`) table options, by default it shows additional items next to
    #' the table like search box, pagination, etc. Only display the table using
    #' list(dom = '')
    #' @param filter (`character`: `"top"`) filter option, it can be either `"none"`, `"bottom"` or `"top"` (default)
    #'
    #' @return `self`
    initialize = function(data, title = "Table", options = list(scrollX = TRUE), filter = "top") {
      super$initialize()
      private$.reactiveValues$data <- data
      private$.title <- title
      private$.options <- options
      private$.filter <- filter
      return(invisible(self))
    },

    #' @description validate
    #'
    #' @return `self`
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertDataFrame(
        .var.name = "data",
        x = isolate(private$.reactiveValues$data),
        add = assertions
      )
      checkmate::assertList(
        .var.name = "options",
        x = private$.options,
        add = assertions
      )
      checkmate::assertTRUE(
        .var.name = "filter",
        x = private$.filter %in% c("none", "bottom", "top"),
        add = assertions
      )
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

    #' UI
    #'
    #' @param title (`character(1)`) Title to use for the DataTable.
    #'
    #' @return `shiny.tag.list`
    UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        DT::DTOutput(outputId = shiny::NS(private$.namespace, "table")),
        shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlButton"), label = "csv")
      )
    },

    #' server
    #'
    #' @param input (`input`)
    #' @param output (`output`)
    #' @param session (`session`)
    #'
    #' @return `NULL`
    server = function(input, output, session) {
      shiny::moduleServer(id = private$.moduleId, module = function(input, output, session) {
        private$renderTable(output)
        private$downloader(output)
        private$setReactiveValues(input)
      })
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .title = "",
    .options = NULL,
    .filter = NULL,
    .reactiveValues = shiny::reactiveValues(
      data = NULL
    ),
    .bindings = shiny::reactiveValues(
      cell_clicked = NULL,
      cells_selected = NULL,
      cell_info = NULL,
      rows_current = NULL,
      rows_all = NULL,
      rows_selected = NULL,
      row_last_clicked = NULL,
      columns_selected = NULL,
      search = NULL,
      search_columns = NULL,
      state = NULL
    ),

    ## Methods ----
    setReactiveValues = function(input) {
      shiny::observeEvent(eventExpr = input$table_cells_selected, {
        private$.bindings$cells_selected <- input$table_cells_selected
      })

      shiny::observeEvent(eventExpr = input$table_cell_clicked, {
        private$.bindings$cell_clicked <- input$table_cell_clicked
      })

      shiny::observeEvent(eventExpr = input$table_cell_info, {
        private$.bindings$cell_info <- input$table_cell_info
      })

      shiny::observeEvent(eventExpr = input$table_rows_all, {
        private$.bindings$rows_all <- input$table_rows_all
      })

      shiny::observeEvent(eventExpr = input$table_rows_current, {
        private$.bindings$rows_current <- input$table_rows_current
      })

      shiny::observeEvent(eventExpr = input$table_rows_selected, {
        private$.bindings$rows_selected <- input$table_rows_selected
      })

      shiny::observeEvent(eventExpr = input$table_row_last_clicked, {
        private$.bindings$row_last_clicked <- input$table_row_last_clicked
      })

      shiny::observeEvent(eventExpr = input$table_columns_selected, {
        private$.bindings$columns_selected <- input$table_columns_selected
      })

      shiny::observeEvent(eventExpr = input$table_search, {
        private$.bindings$search <- input$table_search
      })

      shiny::observeEvent(eventExpr = input$table_search_columns, {
        private$.bindings$search_columns <- input$table_search_columns
      })

      shiny::observeEvent(eventExpr = input$table_state, {
        private$.bindings$state <- input$table_state
      })
    },

    renderTable = function(output) {
      output$table <- DT::renderDT(
        expr = private$.reactiveValues$data,
        filter = private$.filter,
        options = private$.options
      )
    },

    downloader = function(output) {
      output$dlButton <- shiny::downloadHandler(
        filename = private$dlFilename,
        content = private$dlContent
      )
    },

    dlFilename = function() {
      return(sprintf("%s.csv", private$.title))
    },

    dlContent = function(file) {
      write.csv(isolate(private$.reactiveValues$data), file)
    }
  )
)
