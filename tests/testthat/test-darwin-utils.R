test_that("DARWIN: Header", {
  expect_s3_class(darwinHeader(), "shiny.tag.list")
})

test_that("DARWIN: Footer", {
  expect_s3_class(darwinFooter(type = "bslib"), "shiny.tag")
  expect_s3_class(darwinFooter(type = "shinydashboard"), "shiny.tag")
  testthat::expect_error(darwinFooter(type = "none"))
  testthat::expect_error(darwinFooter(type = NULL))
})
