makeFacetFormula = function(facetX, facetY) {
  f <- sprintf(
    "%s ~ %s",
    paste(facetY, collapse = " + "),
    paste(facetX, collapse = " + ")
  )

  if (f == " ~ ") {
    NULL
  } else if (stringr::str_detect(string = f, pattern = "^ \\~")) {
    as.formula(paste0(".", f))
  } else if (stringr::str_detect(string = f, pattern = "\\~ $")) {
    as.formula(paste0(f, "."))
  } else {
    as.formula(f)
  }
}
