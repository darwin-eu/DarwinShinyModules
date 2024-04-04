Incidence <- R6::R6Class(
  classname = "Incidence",
  inherit = Module,

  # Public ----
  public = list(
    initialize = function(appId, incidence) {
      super$initialize(appId)
      private$.incTable <- Table$new(appId = appId, data = incidence)
      private$.incPlot <- Plot$new(
        appId = appId,
        plot = private$plot()
      )
    },

    UI = function(title = "Table") {
      shiny::tagList(
        private$.incPlot$UI(title = NULL),
        private$.incTable$UI(title = NULL)
      )
    },

    server = function(input, output, session) {
      private$.incPlot$server(input, output, session)
      private$.incTable$server(input, output, session)
    }
  ),

  # Private ----
  private = list(
    .incTable = NULL,
    .incPlot = NULL,

    plot = function() {
      classes <- c("IncidencePrevalenceResult", "IncidenceResult")
      incData <- if (!any(class(private$.incTable$data) %in% classes)) {
        data <- dplyr::as_tibble(private$.incTable$data)
        class(data) <- c(classes, class(private$.incTable$data))
        data
      } else {
        private$.incTable$data
      }
      shiny::req(incData)
      plotly::ggplotly(IncidencePrevalence::plotIncidence(incData))
    }
  ),

  # Active ----
  active = list(
    incTable = function() return(private$.incTable),
    incPlot = function() return(private$.incPlot)
  )
)
