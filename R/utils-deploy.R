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
#' @export
#'
#' @return `NULL` invisible
saveAppStructure <- function(appStructure, filePath = "./appStructure.qs", ...) {
  qs2::qs_save(appStructure, file = filePath, ...)
}

#' loadAppStructure
#'
#' Loads an `appStructure` object from a downloaded tar.gz file.
#'
#' @param filePath (`character(1)`) Path to the tar.gz file downlaoded from
#' posit connect.
#'
#' @param appStructureFileName (`character(1)`) Name of the appStructure file.
#'
#' @param subDir (`character(1)`: `NULL`) Optional deviation in the sub
#' directory, if the tar.gz is not structured identically to how posit connect
#' structures these files.
#'
#' @export
#'
#' @returns `list` appStructure
loadAppStructure <- function(filePath, appStructureFileName = "appStructure.qs", subDir = NULL) {
  tmpDir <- tempfile()

  subDir <- if (is.null(subDir)) {
    "."
  } else {
    subDir
  }

  utils::untar(filePath, files = file.path(subDir, appStructureFileName), exdir = tmpDir, )
  qs2::qs_read(file.path(tmpDir, appStructureFileName))
}

#' deployAppStructure
#'
#' Wrapper for `rsconnect::deployApp`. Deploys a shiny app from the appStructure
#'
#' @param appStructure (`list`: `NULL`) appStructure object, i.e. a named list
#' of ShinyModule objects or (named) lists containing (named) ShinyModule
#' objects. Representing the navigation (side) bar of the shiny app.
#'
#' @param appDir (`character(1)`: `tempfile()`) File path to the directory where app files will
#' be written to, and the app is being deployed from. See `rsconnect::deployApp`
#' for further details.
#'
#' @param pkgs (`character(n)`: `NULL`) Additional packages that should be
#' installed and loaded when deploying.
#'
#' @param ... Arguments for `rsconnect::deployApp`
#' @param launchFun (`character(1)`: `"launchDarwinBslibApp"`) Launch function as character
#' @param daemons (`numeric(1)`: `4`) Number of daemons (sub-processes) to launch with the shiny app.
#'
#' @export
#'
#' @return `NULL`
deployAppStructure <- function(appStructure, appDir = tempfile(), launchFun = "launchDarwinBslibApp", daemons = 4, pkgs = NULL, ...) {
  dir.create(appDir, recursive = TRUE)
  saveAppStructure(appStructure, filePath = file.path(appDir, "appStructure.qs"))

  appR <- system.file(package = "DarwinShinyModules", "deployFiles", "app.R")

  lines <- c(
    "library(DarwinShinyModules)",
    sprintf("launchFromDisk(\"appStructure.qs\", launchFun = %s, daemons = %s)", launchFun, daemons)
  )

  writeLines(
    text = c(sprintf("library(%s)", pkgs), lines),
    con = file.path(appDir, "app.R")
  )

  rsconnect::deployApp(appDir = appDir, ...)
}

#' launchFromDisk
#'
#' Launches the shiny app from appStructure object from either a qs-file, or
#' from the object directly, using the provided `launchFun`. When the qs-file
#' does not exist, but the appStructure does, the appStructure is saved to
#' disk, to the provided `fileName`.
#'
#' @param filePath (`character`: `"./appStructure.qs"`) Path to the qs-file,
#' containing the `appStructure`, or where to write the `appStructure` to.
#'
#' @param launchFun (`launchFun`: `DarwinShinyModules::launchDarwinBslibApp`)
#' launch function to use, may be any function that launches a shiny app from
#' an appStructure object.
#'
#' @param daemons (`numeric`: 2) Number of sub R proccess to kick off to load
#' in results asynchronously.
#'
#' @export
#'
#' @return `shiny.appobj` when the app is found, otherwise returns `NULL` invisible
launchFromDisk <- function(
    filePath,
    launchFun = DarwinShinyModules::launchDarwinBslibApp,
    daemons = 4
) {
  if (file.exists(filePath)) {
    appStructure <- qs2::qs_read(filePath)
    mirai::daemons(daemons)
    app <- launchFun(appStructure)
    return(app)
  } else {
    message(sprintf("There is no file '%s'", filePath))
    return(invisible(NULL))
  }
}
