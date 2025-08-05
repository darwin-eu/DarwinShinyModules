DrugRestart <- R6::R6Class(
  classname = "DrugRestart",
  inherit = DarwinShinyModules::ShinyModule,
  public = list(
    initialize = function() {
      super$initialize()
      private$assertInstall("DrugUtilisation", "1.0.1")
    }
  )
)

DrugRestart <- R6::R6Class(
  classname = "DrugRestart",
  inherit = DrugUtilisation,
  active = list(
    #' @field data Data
    data = function() {
      return(private$.data)
    }
  ),

  public = list(
    initialize = function(data, title = "") {
      super$initialize()
      private$.title <- title
      private$.data <- data
      private$initTable()
      private$initPlot()
    }
  ),
  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .title = "",

    ## Modules ----
    .table = NULL,
    .plot = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        private$.plot$UI(),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
      private$.plot$server(input, output, session)
    },

    initTable = function() {
      private$.table <- DarwinShinyModules::GTTable$new(
        fun = DrugUtilisation::tableDrugRestart,
        args = list(result = private$.data)
      )
      private$.table$parentNamespace <- self$namespace
    },

    initPlot = function() {
      private$.plot <- DarwinShinyModules::PlotStatic$new(
        fun = DrugUtilisation::plotDrugRestart,
        args = list(result = private$.data, facet = "age_group", colour = "pregnancy_period"),
        title = private$.title
      )
      private$.plot$parentNamespace <- self$namespace
    }
  )
)
