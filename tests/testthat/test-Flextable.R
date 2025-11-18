test_that("Creation", {
  flexTable <- Flextable$new(
    fun = flextable::flextable,
    args = list(data = iris)
  )

  expect_identical(class(flextable), c("Flextable", "ShinyModule", "R6"))

  expect_identical(flexTable$args, list(data = iris))
  expect_identical(flexTable$fun, flextable::flextable)
})
