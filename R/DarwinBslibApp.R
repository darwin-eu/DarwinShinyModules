DarwinBslibApp <- R6::R6Class(
  classname = "DarwinBslibApp",
  inherit = BslibApp,

  ## Public ----
  public = list(
    UI = function() {
      shiny::addResourcePath(
        prefix = "www",
        directoryPath = system.file("www", package = "DarwinShinyModules")
      )

      shiny::tagList(
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
        ),
        darwinHeader(),
        bslib::page(
          title = private$.title,
          theme = private$theme(),
          fillable = FALSE,
          do.call(bslib::navset_bar, private$parseModules()),
          darwinFooter()
        )
      )
    }
  ),

  ## Private ----
  private = list(
    ### Methods ----
    theme = function() {
      bslib::bs_theme(
        version = 5,
        "navbar-bg" = "#003194"
      )
    }
  )
)
