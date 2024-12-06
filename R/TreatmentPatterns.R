TreatmentPatterns <- R6::R6Class(
  classname = "TreatmentPatterns",
  inherit = ShinyModule,

  active = list(
    #' @field jsShowLegend (`character(1)`) JavaScript function to show legend
    #' on render as text.
    jsShowLegend = function() return(private$.jsShowLegend)
  ),

  public = list(
    #' @field colours (`list()`) Named list of names (domain) and hex colour
    #' codes (range):\cr `list(domain = c("A", "B"), range = c("#FF0000", "#00FF00"))`.
    #' See \link[sunburstR]{sunburst}
    colours = NULL,

    initialize = function(treatmentPathways) {
      private$.treatmentPathways <- treatmentPathways
      super$initialize()
      private$initInputPanel()
      private$.sunburst <- PlotWidget$new(data = treatmentPathways, fun = private$plotSunburst, title = NULL)
      private$.sunburst$parentNamespace <- self$namespace
      private$.sankey <- PlotWidget$new(data = treatmentPathways, fun = private$plotSankey, title = NULL)
      private$.sankey$parentNamespace <- self$namespace
      private$.table <- Table$new(data = treatmentPathways, title = NULL)
      private$.table$parentNamespace <- self$namespace
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .treatmentPathways = NULL,
    .inputPanel = NULL,
    .sunburst = NULL,
    .sankey = NULL,
    .table = NULL,
    .jsShowLegend = "
    function(el, x) {
      d3.select(el).select('.sunburst-togglelegend').property('checkeda', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    ",

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::column(
          width = 4,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 8,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Sunburst Plot",
              private$.sunburst$UI()
            ),
            shiny::tabPanel(
              title = "Sankey Diagram",
              private$.sankey$UI()
            )
          )
        ),
        private$.table$UI()
      )
    },

    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.sunburst$server(input, output, session)
      private$.sankey$server(input, output, session)
      private$.table$server(input, output, session)
      observeEvent(private$.inputPanel$inputValues$none, {
        none <- if (private$.inputPanel$inputValues$none) {
          ""
        } else {
          "None"
        }
        data <- private$.treatmentPathways %>%
          dplyr::filter(.data$path != none)
        # print(data)
        private$.sunburst$data <- data
        # print(private$.sunburst$data)
        private$.sankey$data <- data
      })
    },

    initInputPanel = function() {
      private$.inputPanel <- InputPanel$new(
        funs = list(
          none = shiny::checkboxInput,
          groupCombi = shiny::checkboxInput,
          ageGroup = shinyWidgets::pickerInput,
          sexGroup = shinyWidgets::pickerInput,
          yearGroup = shinyWidgets::pickerInput,
          freqSlider = shiny::sliderInput
        ),
        args = list(
          none = list(
            inputId = "none",
            label = "Show none paths",
            value = TRUE
          ),
          groupCombi = list(
            inputId = "groupCombi",
            label = "Group Combinations",
            value = TRUE
          ),
          ageGroup = list(
            inputId = "ageGroup",
            choices = unique(private$.treatmentPathways$age),
            label = "Age Group"
          ),
          sexGroup = list(
            inputId = "sexGroup",
            choices = unique(private$.treatmentPathways$sex),
            label = "Sex"
          ),
          yearGroup = list(
            inputId = "yearGroup",
            choices = unique(private$.treatmentPathways$indexYear),
            label = "Index Year"
          ),
          freqSlider = list(
            inputId = "freqSlider",
            label = "Frequency",
            min = min(private$.treatmentPathways$freq),
            max = max(private$.treatmentPathways$freq),
            value = c(min(private$.treatmentPathways$freq), max(private$.treatmentPathways$freq)),
            dragRange = TRUE,
            step = 1
          )
        )
      )
      private$.inputPanel$parentNamespace <- self$namespace
    },

    plotSankey = function(data) {
      req(all(
        nrow(data) > 0,
        !is.null(data)
      ))
      TreatmentPatterns::createSankeyDiagram(
        treatmentPathways = data,
        groupCombinations = FALSE,
        colors = self$colours
      )
    },

    plotSunburst = function(data) {
      req(all(
        nrow(data) > 0,
        !is.null(data)
      ))
      htmlwidgets::onRender(
        TreatmentPatterns::createSunburstPlot(
          treatmentPathways = data,
          groupCombinations = FALSE,
          legend = list(w = 400),
          withD3 = TRUE,
          colors = self$colours
        ),
        jsCode = private$.jsShowLegend
      )
    }
  )
)
