test_that("Creation", {
  f <- function(data) {
    gg <- ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_bw()

    ggplotly(gg)
  }

  plot <- suppressWarnings(PlotPlotly$new(fun = f, args = list(data = iris)))

  expect_identical(class(plot), c("PlotPlotly", "Plot", "ShinyModule", "R6"))

  pg <- plot$fun(plot$args$data)
  expect_identical(class(pg), c("plotly", "htmlwidget"))
})

test_that("App", {
  f <- function(data) {
    gg <- ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_bw()

    ggplotly(gg)
  }

  plot <- suppressWarnings(PlotPlotly$new(fun = f, args = list(data = iris)))

  modServer <- function(id) {
    plot$server(input, output, session)
  }

  testServer(modServer, {
    suppressWarnings({
      ## ReactiveValues ----
      expect_identical(
        isolate(plot$args$data),
        iris
      )

      ## Outputs ----
      expect_true(nchar(output$plot) > 0)
    })
  })
})
