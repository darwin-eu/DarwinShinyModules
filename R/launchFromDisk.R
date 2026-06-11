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
#' @return `shiny.appobj` when the app is found, otherwise returns `NULL` invisible
launchFromDisk <- function(
    filePath,
    launchFun = DarwinShinyModules::launchDarwinBslibApp,
    daemons = 2
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
