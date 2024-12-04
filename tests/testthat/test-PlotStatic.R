test_that("Creation", {
  f <- function(data) {
      ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        theme_bw()
  }

  plot <- PlotStatic$new(data = iris, fun = f, title = "Iris")

  expect_identical(class(plot), c("PlotStatic", "Plot", "ShinyModule", "R6"))

  g <- plot$fun(data = plot$data)
  expect_identical(class(g), c("gg", "ggplot"))

  f <- function(data, foo = "bar") {
    ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_bw() +
      labs(title = foo)
  }

  plot <- PlotStatic$new(data = iris, fun = f)
  g <- plot$fun(data = plot$data)
  expect_identical(g$labels$title, "bar")
})

test_that("App", {
  f <- function(data) {
    ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_bw()
  }

  plot <- PlotStatic$new(data = iris, fun = f, title = "Iris")

  modServer <- function(id) {
    plot$server(input, output, session)
  }

  testServer(modServer, {
    ## ReactiveValues ----
    expect_identical(
      isolate(plot$reactiveValues$data),
      iris
    )

    ## Outputs ----
    expect_true(nchar(output$plot$src) > 0)
  })
})
