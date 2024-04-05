#' @title Table
#'
#' @include Module.R
#'
#' @description
#' Table Module
#'
#' @field dataTableBindings (`reactivevalues`) Bindings of the DataTable in a reactive environment.
#' @field data The data to render in the DataTable, usually a `data.frame`-like object.
#'
#' @export
Table <- R6::R6Class(
  classname = "Table",
  inherit = Module,

  # Public ----
  public = list(
    ## Methods ----
    #' initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param fun Function to plot with, with one argument: `data`.
    #'
    #' @return `self`
    initialize = function(appId, data) {
      super$initialize(appId)
      private$.data <- data
      return(invisible(self))
    },

    #' validate
    #'
    #' @return `self`
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

    #' UI
    #'
    #' @param title (`character(1)`) Title to use for the DataTable.
    #'
    #' @return `shiny.tag.list`
    UI = function(title = "Table") {
      shiny::tagList(
        shiny::h3(title),
        DT::DTOutput(outputId = shiny::NS(private$.appId, private$id("table"))),
        shiny::downloadButton(outputId = shiny::NS(private$.appId, private$id("dlButton")), label = "csv")
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
      private$renderTable(output)
      private$downloader(output)
      private$setReactiveValues(input)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .dataTableBindings = shiny::reactiveValues(
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
        private$.dataTableBindings$cell_clicked <- input[[cellClicked]]
        private$.dataTableBindings$cells_selected <- input[[cellsSelected]]
        private$.dataTableBindings$cell_info <- input[[cellInfo]]
        private$.dataTableBindings$rows_current <- input[[rowsCurrent]]
        private$.dataTableBindings$rows_all <- input[[rowsAll]]
        private$.dataTableBindings$rows_selected <- input[[rowsSelected]]
        private$.dataTableBindings$row_last_clicked <- input[[rowLastClicked]]
        private$.dataTableBindings$columns_selected <- input[[columnsSelected]]
        private$.dataTableBindings$search <- input[[search]]
        private$.dataTableBindings$search_columns <- input[[searchColumns]]
        private$.dataTableBindings$state <- input[[state]]
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
    dataTableBindings = function() return(private$.dataTableBindings)
  )
)
#' initialize
#'
#' @param appId (`character(1)`) ID of the app, to use for namespacing.
#' @param data Data to plot with, usually a `data.frame`-like object.
#' @param fun Function to plot with, with one argument: `data`.
#'
#' @return `self`
