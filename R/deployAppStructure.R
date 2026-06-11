#' deployAppStructure
#'
#' Wrapper for `rsconnect::deployApp`. Deploys a shiny app from the appStructure
#'
#' @param appStructure (`list`: `NULL`) appStructure object, i.e. a named list
#' of ShinyModule objects or (named) lists containing (named) ShinyModule
#' objects. Representing the navigation (side) bar of the shiny app.
#'
#' @param appDir (`character`) File path to the directory where app files will
#' be written to, and the app is being deployed from. See `rsconnect::deployApp`
#' for further details.
#'
#' @param ... Arguments for `rsconnect::deployApp`
#'
#' @return `NULL`
deployAppStructure <- function(appStructure, appDir = tempdir(), ...) {
  saveAppStructure(appStructure, filePath = file.path(appDir, "appStructure.qs"))

  # When actually developed, this is sourced with `System.file("./app.R", package = "DarwinShinyModules")`
  appR <- "./app.R"

  file.copy(appR, to = file.path(appDir, "app.R"))

  rsconnect::deployApp(appDir = appDir, ...)
}
