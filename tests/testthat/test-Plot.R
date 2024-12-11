test_that("Creation", {
  f <- function(data) {}
  plot <- Plot$new(fun = f, args = list(data = iris))

  expect_identical(class(plot), c("Plot", "ShinyModule", "R6"))

  f <- function() {}
  expect_error(Plot$new(data = iris, fun = f))

  f <- function(data, foo = "bar") {}
  plot <- Plot$new(fun = f, args = list(data = iris), title = "Iris")

  expect_true(is.null(plot$reactiveValues))
  expect_true(is.function(plot$fun))
  expect_identical(plot$args$data, iris)
  expect_identical(plot$title, "Iris")
})
