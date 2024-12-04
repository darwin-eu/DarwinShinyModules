test_that("Creation", {
  text <- Text$new(
    markdown = c(
      "#H1 Header 1",
      "#H2 Header 2",
      "**bold text**"
    )
  )

  expect_identical(
    text$markdown,
    c(
      "#H1 Header 1",
      "#H2 Header 2",
      "**bold text**"
    )
  )
})
