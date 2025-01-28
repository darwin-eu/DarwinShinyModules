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
        utils::packageVersion("DarwinShinyModules"))
    )
  )
}

#' darwinApp
#'
#' @param appStructure (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#' @param studyStatus (`character(1)`: `"ongoing"`) Status of the study
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(ggplot2)
#'
#' mtcarsTable <- Table$new(data = mtcars)
#' irisTable <- Table$new(data = iris)
#'
#' irisPlotFun <- function(data) {
#'   ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'     geom_point() +
#'     theme_bw()
#' }
#'
#' irisPlot <- PlotStatic$new(fun = irisPlotFun, args = list(data = iris))
#'
#' if (interactive()) {
#'   darwinApp(
#'     moduleList = list(
#'       # Menu item `Iris`
#'       Iris = list(irisPlot, irisTable),
#'       # Menu item `MT Cars`
#'       MT_Cars = list(mtcarsTable)
#'     )
#'   )
#' }
darwinApp <- function(appStructure, title = NULL, studyStatus = "ongoing") {
  assertAppStructure(appStructure)
  checkmate::assertChoice(studyStatus, choices = c("ongoing", "completed", "stop"))
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE)

  shiny::addResourcePath(
    prefix = "www/img",
    directoryPath = system.file("www/img", package = "DarwinShinyModules")
  )

  ui <- shiny::tags$body(
    shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "theme.css")),
    darwinHeader(),
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = title,
        titleWidth = 300,
        StudyStatus(studyStatus)
      ),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          modulesSidebar(appStructure)
        ), width = 300
      ),
      body = shinydashboard::dashboardBody(modulesBody(appStructure))
    ),
    darwinFooter()
  )

  server <- function(input, output, session) {
    modules <- unlist(appStructure)
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}
