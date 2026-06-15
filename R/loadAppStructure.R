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
