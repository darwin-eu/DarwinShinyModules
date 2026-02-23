Indication <- R6::R6Class(
  classname = "Indication",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    data = function() {
      return(private$.data)
    }
  ),

  # Public ----
  public = list(
    initialize = function(result, ...) {
      super$initialize(...)
      private$.result <- result
      private$.table <- Table$new(data = rawDat, title = NULL, filter = "top", parentNamespace = self$namespace)

      private$.tableMod <- DarwinShinyModules::Flextable$new(fun = )
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .result = NULL,
    .table = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(

      )
    },
    .server = function(input, output, session) {
      private$.view_2Mod$server(input, outupt, session)
      output$view_1 <- gt::render_gt({
        DrugUtilisation::tableIndication(private$.data, header = "indication")
      })
    }
  )
)
