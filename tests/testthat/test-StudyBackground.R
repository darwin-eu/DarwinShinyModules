test_that("StudyBackground: creation", {
  studyBackground <- StudyBackground$new(
    background = "./methods.md",
    EUPAS = "EUPAS999999"
  )

  expect_true(
    all(c("StudyBackground", "ShinyModule") %in% class(studyBackground))
  )
})
