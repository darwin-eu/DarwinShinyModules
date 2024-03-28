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
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,

    ## Methods ----
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
    data = function() return(private$.data)
  )
)
