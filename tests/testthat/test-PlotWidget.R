library(htmlwidgets)

test_that("Creation", {
  skip_if_not(suggestsInstalled)

  src <- c(
    "A", "A", "A", "A",
    "B", "B", "C", "C", "D"
  )
  target <- c(
    "B", "C", "D", "J",
    "E", "F", "G", "H", "I"
  )

  networkData <- data.frame(src, target)

  f <- function(data) {
    simpleNetwork(data)
  }

  widget <- PlotWidget$new(fun = f, args = list(data = networkData), title = "Network")

  expect_identical(class(widget), c("PlotWidget", "Plot", "ShinyModule", "R6"))
  widget$fun(data = widget$args$data)
  net <- widget$fun(data = widget$args$data)
  expect_identical(class(net), c("forceNetwork", "htmlwidget"))
})

test_that("App", {
  skip_if_not(suggestsInstalled)

  src <- c(
    "A", "A", "A", "A",
    "B", "B", "C", "C", "D"
  )
  target <- c(
    "B", "C", "D", "J",
    "E", "F", "G", "H", "I"
  )

  networkData <- data.frame(src, target)

  f <- function(data) {
    simpleNetwork(data)
  }

  widget <- PlotWidget$new(fun = f, args = list(data = networkData), title = "Network")

  modServer <- function(id) {
    widget$server(input, output, session)
  }

  testServer(modServer, {
    ## ReactiveValues ----
    expect_identical(
      widget$args$data,
      networkData
    )

    ## Outputs ----
    expect_true(nchar(output$plot$html) > 0)
  })
})
