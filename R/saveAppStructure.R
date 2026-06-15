#' saveAppStructure
#'
#' Wrapper for `qs2::qs_save()`. Saves an `appStructure` object as a qs-file
#' using `qs2`.
#'
#' @param appStructure (`list`: `NULL`) appStructure object, i.e. a named list
#' of ShinyModule objects or (named) lists containing (named) ShinyModule
#' objects. Representing the navigation (side) bar of the shiny app.
#'
#' @param filePath (`character`: `"./appStructure.qs"`) Path to the qs-file,
#' containing the `appStructure`, or where to write the `appStructure` to.
#'
#' @param ... Extra arguments for `qs2::qs_save()`
#'
#' @return `NULL` invisible
saveAppStructure <- function(appStructure, filePath = "./appStructure.qs", ...) {
  qs2::qs_save(appStructure, file = filePath, ...)
}
