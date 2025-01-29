#' launchShinydashboardApp
#'
#' @param appStructure (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' base <- Text$new("**base**")
#' nested_a <- Text$new("**nested A**")
#' nested_b <- Text$new("**nested B**")
#' sub_a <- Text$new("**sub A**")
#' sub_b <- Text$new("**sub B**")
#' comb_a <- Text$new("**comb A**")
#' comb_b <- Text$new("**comb B**")
#' comb_c <- Text$new("**comb C**")
#'
#' if (interactive()) {
#'   appStructure <- list(
#'     base = base,
#'     nested = list(nested_a, nested_b),
#'     nested_sub = list(
#'       sub_a = sub_a,
#'       sub_b = sub_b
#'     ),
#'     nested_combined = list(
#'       comb_a_b = list(comb_a, comb_b),
#'       comb_c = comb_c
#'     )
#'   )
#'
#'   launchShinydashboardApp(appStructure)
#' }
launchShinydashboardApp <- function(appStructure, title = NULL) {
  if (!rlang::is_installed(pkg = "shinydashboard")) {
    rlang::abort("`bslib` is not installed.")
  }
  assertAppStructure(appStructure)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE)
  app <- ShinydashboardApp$new(appStructure)
  app$launch()
}
