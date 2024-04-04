Prevalence <- R6::R6Class(
  classname = "Prevalence",
  inherit = Module,

  # Public ----
  public = list(
    initialize = function(appId, prevalence) {
      super$initialize(appId)
      private$.prevTable <- Table$new(appId = appId, data = prevalence)
      private$.prevPlot <- Plot$new(
        appId = appId,
        plot = private$plot()
      )
    },

    UI = function(title = "Table") {
      shiny::tagList(
        private$.prevPlot$UI(title = NULL),
        private$.prevTable$UI(title = NULL)
      )
    },

    server = function(input, output, session) {
      private$.prevPlot$server(input, output, session)
      private$.prevTable$server(input, output, session)
    }
  ),

  # Private ----
  private = list(
    .prevTable = NULL,
    .prevPlot = NULL,

    plot = function() {
      classes <- c("IncidencePrevalenceResult", "PrevalenceResult")
      prevData <- if (!any(class(private$.prevTable$data) %in% classes)) {
        data <- dplyr::as_tibble(private$.prevTable$data)
        class(data) <- c(classes, class(private$.prevTable$data))
        data
      } else {
        private$.prevTable$data
      }
      plotly::ggplotly(IncidencePrevalence::plotPrevalence(prevData))
    }
  ),

  # Active ----
  active = list(
    prevTable = function() return(private$.prevTable),
    prevPlot = function() return(private$.prevPlot)
  )
)
