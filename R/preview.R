#' Preview
#'
#' Launches a shiny app with the modules' `server()` and `UI()` methods.
#'
#' @param module ([ShinyModule]) A module object.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' table <- Table$new(appId = "id", data = iris)
#'
#' if (interactive()) {
#'   preview(table)
#' }
preview <- function(module) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertR6(
    .var.name = "module",
    x = module,
    classes = c("ShinyModule"),
    add = assertions
  )
  checkmate::reportAssertions(assertions)

  ui <- shiny::fluidPage(
    module$UI()
  )

  server <- function(input, output, session) {
    shiny::moduleServer(id = module$appId, module = function(input, output, session) {
      module$server(input, output, session)
    })
  }

  shiny::shinyApp(ui, server)
}
