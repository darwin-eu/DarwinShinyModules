#' @title Table
#'
#' @include ShinyModule.R
#'
#' @description
#' Table Module
#'
#' @export
Table <- R6::R6Class(
  classname = "Table",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param appId (`character(1)`) ID of the app, to use for namespacing.
    #' @param data Data to plot with, usually a `data.frame`-like object.
    #' @param options table options, by default it shows additional items next to
    #' the table like search box, pagination, etc. Only display the table using
    #' list(dom = '')
    #' @param filter filter option, it can be either "none", "bottom" or "top" (default)
    #'
    #' @return `self`
    initialize = function(appId, data, options = list(scrollX = TRUE), filter = "top") {
      super$initialize(appId)
      private$.data <- data
      private$.options <- options
      private$.filter <- filter
      return(invisible(self))
    },

    #' @description validate
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
      # checkmate::assertList(
      #   .var.name = "options",
      #   x = private$.tableOptions,
      #   add = assertions
      # )
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
    UI = function(title = "Table") {
      shiny::tagList(
        shiny::h3(title),
        DT::DTOutput(outputId = shiny::NS(private$.appId, self$id("table"))),
        shiny::downloadButton(outputId = shiny::NS(private$.appId, self$id("dlButton")), label = "csv")
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

  # Active ----
  active = list(
    #' @field bindings (`reactivevalues`) Bindings of the DataTable in a reactive environment.
    data = function() {
      return(private$.data)
    },

    #' @field data The data to render in the DataTable, usually a `data.frame`-like object.
    bindings = function() {
      return(private$.bindings)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .options = NULL,
    .filter = NULL,
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
      cellClicked <- sprintf("%s_cell_clicked", self$id("table"))
      cellsSelected <- sprintf("%s_cells_selected", self$id("table"))
      cellInfo <- sprintf("%s_cell_info", self$id("table"))
      rowsCurrent <- sprintf("%s_rows_current", self$id("table"))
      rowsAll <- sprintf("%s_rows_all", self$id("table"))
      rowsSelected <- sprintf("%s_rows_selected", self$id("table"))
      rowLastClicked <- sprintf("%s_row_last_clicked", self$id("table"))
      columnsSelected <- sprintf("%s_columns_selected", self$id("table"))
      search <- sprintf("%s_search", self$id("table"))
      searchColumns <- sprintf("%s_search_columns", self$id("table"))
      state <- sprintf("%s_state", self$id("table"))

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
        private$.bindings$cell_clicked <- input[[cellClicked]]
        private$.bindings$cells_selected <- input[[cellsSelected]]
        private$.bindings$cell_info <- input[[cellInfo]]
        private$.bindings$rows_current <- input[[rowsCurrent]]
        private$.bindings$rows_all <- input[[rowsAll]]
        private$.bindings$rows_selected <- input[[rowsSelected]]
        private$.bindings$row_last_clicked <- input[[rowLastClicked]]
        private$.bindings$columns_selected <- input[[columnsSelected]]
        private$.bindings$search <- input[[search]]
        private$.bindings$search_columns <- input[[searchColumns]]
        private$.bindings$state <- input[[state]]
      })
    },

    renderTable = function(output) {
      output[[self$id("table")]] <- DT::renderDT(
        expr = private$.data,
        filter = private$.filter,
        options = private$.options
      )
    },

    downloader = function(output) {
      output[[self$id("dlButton")]] <- shiny::downloadHandler(
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
  )
)
