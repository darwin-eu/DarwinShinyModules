#' @title Incidence
#'
#' @description
#' Incidence Module
#'
#' @export
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

    updateColPlot = function(data) {
      print(private$.incTable)
      # observeEvent(private$.incTable$reactiveValues$columns_selected, {
      #   print(private$.incTable$reactiveValues$columns_selected)
      #   data <- data %>%
      #     dplyr::mutate(
      #       case_when(colour = dplyr::row_number() %in% incPlot) ~ "#0000FF",
      #       .default = "#000000"
      #     )
      # })
      # return(data)
    },

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
      incData <- incData %>%
        dplyr::mutate(colour = "#000000")
      private$updateColPlot(incData)
      plotly::ggplotly(IncidencePrevalence::plotIncidence(incData, colour = "colour"))
    }
  ),

  # Active ----
  active = list(
    incTable = function() return(private$.incTable),
    incPlot = function() return(private$.incPlot)
  )
)
