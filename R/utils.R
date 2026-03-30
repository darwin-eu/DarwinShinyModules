makeFacetFormula = function(facetX, facetY) {
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

convertLabelToLogical = function(label, trueVal = "On", falseVal = "Off") {
  if (label == trueVal) TRUE else if (label == falseVal) FALSE else FALSE
}
