test_that("Creation", {
  tp <- data.frame(
    path = c("A+B-C", "B+C-A", "B-A+C"),
    freq = 100,
    sex = "all",
    age = "all",
    indexYear = "all"
  )

  tpMod <- TreatmentPatterns$new(treatmentPathways = tp)

  expect_identical(class(tpMod), c("TreatmentPatterns", "ShinyModule", "R6"))

  tpMod$sunburstCols <- list(
    domain = list("A", "B", "C", "A+B", "B+C", "A+C"),
    range = list("#ff0000", "#aa0000", "#770000", "#0000ff", "#0000aa", "#000077"))

  tpMod$sankeyCols <- list(
    A = "#ff0000",
    B = "#aa0000",
    C = "#770000",
    `A+B` = "#0000ff",
    `B+C` = "#0000a",
    `A+C` = "#000077"
  )

  expect_true(all(names(tpMod$sankeyCols) %in% c("A", "B", "C", "A+B", "B+C", "A+C")))
  expect_true(all(tpMod$sunburstCols$domain %in% c("A", "B", "C", "A+B", "B+C", "A+C")))
})
