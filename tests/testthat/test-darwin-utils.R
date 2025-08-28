test_that("DARWIN: Header", {
  expect_s3_class(darwinHeader(), "shiny.tag.list")
})

test_that("DARWIN: Header", {
  expect_s3_class(darwinFooter(), "shiny.tag")
})
