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
#' table <- Table$new(appId = "id", data = iris)
#'
#' if (interactive()) {
#'   preview(table)
#' }
preview <- function(modules) {
  assertions <- checkmate::makeAssertCollection()
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
    shiny::moduleServer(id = modules[[1]]$appId, module = function(input, output, session) {
      ids <- unique(unlist(lapply(modules, function(module) {
        module$appId
      })))
      if (length(ids) > 1) {
        warning(sprintf("Moduels have different appIds: (%s), using '%s'", paste(sprintf("'%s'", ids), collapse = ", "), modules[[1]]$appId))
      }

      servASync <- lapply(modules, function(module) {
        promises::future_promise(module$server(input, output, session))
      })
    })
  }

  shiny::shinyApp(ui, server)
}
