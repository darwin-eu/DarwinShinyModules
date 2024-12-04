#' Preview
#'
#' Launches a shiny app with the modules' `server()` and `UI()` methods.
#'
#' @param modules ([ShinyModule]) A vector of module objects.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' table <- Table$new(data = iris)
#'
#' if (interactive()) {
#'   preview(table)
#' }
preview <- function(modules) {
  assertions <- checkmate::makeAssertCollection()
  modules <- if (all(class(modules) != "list")) {
    list(modules)
  } else {
    modules
  }

  checkmate::assertList(
    .var.name = "modules",
    x = modules,
    types = "ShinyModule",
    min.len = 1,
    add = assertions
  )

  checkmate::reportAssertions(assertions)

  ui <- shiny::fluidPage(
    shiny::tagList(
      lapply(modules, function(module) {
        module$UI()
      })
    )
  )

  server <- function(input, output, session) {
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}
