App <- R6::R6Class(
  classname = "App",

  active = list(),
  ## Public ----
  public = list(
    ## Methods ----
    initialize = function(appStructure) {
      private$.appStructure <- appStructure
    },

    launch = function() {
      shiny::shinyApp(ui = self$UI(), server = self$server)
    },

    server = function(input, output, session) {},
    UI = function() {}
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .appStructure = list()
  )
)
