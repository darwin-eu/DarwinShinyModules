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

  widget <- PlotWidget$new(data = networkData, fun = f, title = "Network")

  expect_identical(class(widget), c("PlotWidget", "Plot", "ShinyModule", "R6"))

  net <- widget$fun(data = widget$data)
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

  widget <- PlotWidget$new(data = networkData, fun = f, title = "Network")

  modServer <- function(id) {
    widget$server(input, output, session)
  }

  testServer(modServer, {
    ## ReactiveValues ----
    expect_identical(
      isolate(widget$reactiveValues$data),
      networkData
    )

    ## Outputs ----
    expect_true(nchar(output$plot$html) > 0)
  })
})
