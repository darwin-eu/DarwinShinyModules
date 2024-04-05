#' @title Table
#'
#' @description
#' Table Module
#'
#' @export
Table <- R6::R6Class(
  classname = "Table",
  inherit = Module,

  # Public ----
  public = list(
    ## Override ----
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.data <- data
    },

    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "appId",
        x = private$.appId,
        len = 1
      )
      checkmate::assertDataFrame(
        .var.name = "data",
        x = private$.data,
        add = assertions
      )
      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

    ## Methods ----
    UI = function(title = "Table") {
      shiny::tagList(
        shiny::h3(title),
        DT::DTOutput(outputId = shiny::NS(private$.appId, private$id("table"))),
        shiny::downloadButton(outputId = shiny::NS(private$.appId, private$id("dlButton")), label = "csv")
      )
    },

    server = function(input, output, session) {
      private$renderTable(output)
      private$downloader(output)
      private$setReactiveValues(input)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .reactiveValues = shiny::reactiveValues(
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
      cellClicked <- sprintf("%s_cell_clicked", private$id("table"))
      cellsSelected <- sprintf("%s_cells_selected", private$id("table"))
      cellInfo <- sprintf("%s_cell_info", private$id("table"))
      rowsCurrent <- sprintf("%s_rows_current", private$id("table"))
      rowsAll <- sprintf("%s_rows_all", private$id("table"))
      rowsSelected <- sprintf("%s_rows_selected", private$id("table"))
      rowLastClicked <- sprintf("%s_row_last_clicked", private$id("table"))
      columnsSelected <- sprintf("%s_columns_selected", private$id("table"))
      search <- sprintf("%s_search", private$id("table"))
      searchColumns <- sprintf("%s_search_columns", private$id("table"))
      state <- sprintf("%s_state", private$id("table"))

      observeEvent(
        c(
          input[[cellClicked]],
          input[[cellsSelected]],
          input[[cellInfo]],
          input[[rowsCurrent]],
          input[[rowsAll]],
          input[[rowsSelected]],
          input[[rowLastClicked]],
          input[[columnsSelected]],
          input[[search]],
          input[[searchColumns]],
          input[[state]]
        ), {
        private$.reactiveValues$cell_clicked <- input[[cellClicked]]
        private$.reactiveValues$cells_selected <- input[[cellsSelected]]
        private$.reactiveValues$cell_info <- input[[cellInfo]]
        private$.reactiveValues$rows_current <- input[[rowsCurrent]]
        private$.reactiveValues$rows_all <- input[[rowsAll]]
        private$.reactiveValues$rows_selected <- input[[rowsSelected]]
        private$.reactiveValues$row_last_clicked <- input[[rowLastClicked]]
        private$.reactiveValues$columns_selected <- input[[columnsSelected]]
        private$.reactiveValues$search <- input[[search]]
        private$.reactiveValues$search_columns <- input[[searchColumns]]
        private$.reactiveValues$state <- input[[state]]
      })
    },

    renderTable = function(output) {
      output[[private$id("table")]] <- DT::renderDT(
        expr = private$.data,
        filter = "top",
        options = list(scrollX = TRUE)
      )
    },

    downloader = function(output) {
      output[[private$id("dlButton")]] <- shiny::downloadHandler(
        filename = private$dlFilename,
        content = private$dlContent
      )
    },

    dlFilename = function() {
      return("table.csv")
    },

    dlContent = function(file) {
      write.csv(private$.data, file)
    }
  ),

  # Active ----
  active = list(
    data = function() return(private$.data),
    reactiveValues = function() return(private$.reactiveValues)
  )
)
