makeFacetFormula <- function(facetX, facetY) {
  f <- sprintf(
    "%s ~ %s",
    paste(facetY, collapse = " + "),
    paste(facetX, collapse = " + ")
  )

  if (f == " ~ ") {
    NULL
  } else if (stringr::str_detect(string = f, pattern = "^ \\~")) {
    stats::as.formula(paste0(".", f))
  } else if (stringr::str_detect(string = f, pattern = "\\~ $")) {
    stats::as.formula(paste0(f, "."))
  } else {
    stats::as.formula(f)
  }
}

convertLabelToLogical <- function(label, trueVal = "On", falseVal = "Off") {
  if (label == trueVal) TRUE else if (label == falseVal) FALSE else FALSE
}

nullToDefault <- function(value, default) {
  if (is.null(value)) default else value
}

ggThemeDarwin <- function(fontSize = NULL) {
  list(
    visOmopResults::themeVisOmop(style = "darwin", fontsizeRef = fontSize),
    ggplot2::theme(strip.text.y.right = ggplot2::element_text(angle = -90))
  )
}

#' getCDMAcronyms
#'
#' Returns a character vector containing all the data source acronyms from the DARWIN EU Portal
#'
#' @returns `character(n)`
#' @export
#'
#' @examples
#' getCDMAcronyms()
getCDMAcronyms <- function() {
  readRDS(system.file("datapartners.RDS", package = "DarwinShinyModules")) |>
    dplyr::filter(.data$field == "db_acrynym") |>
    dplyr::select("Answer") |>
    dplyr::pull()
}
