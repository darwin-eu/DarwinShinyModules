test_that("Preview", {
  table <- Table$new(data = iris)
  text <- Text$new(markdown = "This is a vert nice table of Iris data.")

  app <- preview(list(table, text))

  expect_identical(class(app), "shiny.appobj")
})
