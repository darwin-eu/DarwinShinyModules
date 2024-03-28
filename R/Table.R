Table <- R6::R6Class(
  classname = "Table",
  inherit = Module,

  # Public ----
  public = list(
    ## Override ----
    initialize = function(appId, dataPath = "", data = NULL) {
      super$initialize(appId)
      private$.dataPath <- dataPath
      private$.data <- data
      self$validate()
    },

    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "dataPath",
        x = private$.dataPath,
        len = 1,
        add = assertions
      )
      checkmate::assertDataFrame(
        .var.name = "data",
        x = private$.data,
        null.ok = TRUE,
        add = assertions
      )
      checkmate::reportAssertions(assertions)
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
      private$readData()
      private$renderTable(output)
      private$downloader(output)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .dataPath = "",
    .data = NULL,

    ## Methods ----
    readData = function() {
      if (!is.null(private$.data) & nchar(private$.dataPath) > 0) {
        warning(sprintf("Data was read in with `new(appId = %s, data = ...)`. Overwriting data.", private$.appId))
      }
      private$.data <- read.csv(private$.dataPath)
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
      return(basename(private$.dataPath))
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
