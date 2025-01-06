test_that("Creation", {
  gtTable <- GTTable$new(
    fun = gt::gt,
    args = list(data = iris)
  )

  expect_identical(class(gtTable), c("GTTable", "ShinyModule", "R6"))

  expect_identical(gtTable$args, list(data = iris))
  expect_identical(gtTable$fun, gt::gt)
})
