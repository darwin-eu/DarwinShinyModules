DrugUtilisation <- R6::R6Class(
  classname = "DrugUtilisation",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    data = function() {
      return(private$.data)
    }
  ),

  # Public ----
  public = list(
    initialize = function(data) {
      super$initialize()
      private$.data <- data
      private$.view_2Mod <- Table$new(data = omopgenerics::tidy(private$.data), title = NULL, filter = "top")
      private$.view_2Mod$parentNamespace <- self$namespace
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .view_2Mod = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "View 1",
            gt::gt_output(outputId = shiny::NS(self$namespace, "view_1"))
          ),
          shiny::tabPanel(
            title = "View 2",
            private$.view_2Mod$UI()
          )
        )
      )
    },
    .server = function(input, output, session) {
      private$.view_2Mod$server(input, outupt, session)
      output$view_1 <- gt::render_gt({
        DrugUtilisation::tableDrugUtilisation(private$.data, groupColumn = "ingredient")
      })
    }
  )
)
