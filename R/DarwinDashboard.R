# Classes ----
DarwinDashboard <- R6::R6Class(
  classname = "DarwinDashboard",
  inherit = ShinydashboardApp,

  ## Public ----
  public = list(
    ### Methods ----
    initialize = function(appStructure, title = "", studyStatus = "ongoing") {
      super$initialize(appStructure, title)
      private$.studyStatus <- studyStatus
    },

    launch = function() {
      shiny::addResourcePath(
        prefix = "www/img",
        directoryPath = system.file("www/img", package = "DarwinShinyModules")
      )
      super$launch()
    },

    UI = function() {
      shiny::tags$body(
        shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "theme.css")),
        darwinHeader(),
        shinydashboard::dashboardPage(
          header = shinydashboard::dashboardHeader(
            title = private$.title,
            titleWidth = 300,
            studyStatus(private$.studyStatus)
          ),
          sidebar = shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              modulesSidebar(private$.appStructure)
            ), width = 300
          ),
          body = shinydashboard::dashboardBody(modulesBody(private$.appStructure))
        ),
        darwinFooter()
      )
    },

    server = function(input, output, session) {
      modules <- unlist(private$.appStructure)
      for (module in modules) {
        module$server(input, output, session)
      }
    }
  ),

  ## Private ----
  private = list(
    ## Fields ----
    .studyStatus = "ongoing"
  )
)

# Internal Functions ----
darwinHeader <- function() {
  shiny::tagList(
    shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "style.css")),
    shiny::tags$body(
      shiny::tags$div(
        class = "g-block size-100 darwin-top-line",
        shiny::tags$div(
          id = "custom-2346-particle",
          class = "g-content g-particle"
        )
      ),
      shiny::tags$div(
        class = "g-grid",
        shiny::tags$div(
          class = "header-background-image",
          style = "padding-top&#x3A;&#x20;40px&#x20;&#x21;important&#x3B;&#x20;padding-left&#x3A;&#x20;25px&#x20;&#x21;important&#x3B;",
          shiny::tags$div(
            id = "logo-1935-particle",
            class = "g-content g-particle",
            shiny::tags$a(
              href = "/", class = "g-logo darwin-eu-logo", target = "_self", title = "", rel = "home",
              shiny::tags$img(
                src = "https://darwin-eu.org/templates/rt_horizon/custom/images/darwin-eu-logo.png",
                height = "100px",
                alt = ""
              )
            )
          )
        )
      )
    )
  )
}

darwinFooter <- function() {
  shiny::tags$footer(
    shiny::h6(
      sprintf(
        "Generated with DarwinShinyModules %s",
        utils::packageVersion("DarwinShinyModules")
      )
    )
  )
}
