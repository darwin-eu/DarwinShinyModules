test_that("Creation", {
  table <- Table$new(iris)
  inputPanel <- InputPanel$new(
    funs = list(
    inputSpecies = shiny::selectInput
    ),
    args = list(
      inputSpecies = list(
        inputId = "inputSpecies",
        label = "Select Species",
        choices = unique(iris$Species),
        selected = unique(iris$Species)[1]
      )
    )
  )

  f <- function() {}

  bridge <- Bridge$new(inputPanel, table, bridgeFun = f)

  expect_true(class(bridge$birdgeFun) == "function")
  for (module in bridge$modules) {
    expect_true("ShinyModule" %in% class(module))
  }
})

test_that("App", {
  table <- Table$new(iris)
  inputPanel <- InputPanel$new(
    funs = list(
      inputSpecies = shiny::selectInput
    ),
    args = list(
      inputSpecies = list(
        inputId = "inputSpecies",
        label = "Select Species",
        choices = unique(iris$Species),
        selected = unique(iris$Species)[1]
      )
    )
  )

  bridgeFun <- function(input, output, session) {
    shiny::observeEvent(inputPanel$inputValues$inputSpecies, {
      table$data <- iris %>%
        dplyr::filter(.data$Species == inputPanel$inputValues$inputSpecies)
    })
  }

  bridge <- Bridge$new(inputPanel, table, bridgeFun = bridgeFun)

  modServer <- function(id) {
    bridge$server(input, output, session)
  }

  for (flower in c("setosa", "versicolor", "virginica")) {
    testServer(modServer, {
      args <- list(flower)
      names(args) <- shiny::NS(bridge$modules[[1]]$moduleId, "inputSpecies")
      do.call(what = session$setInputs, args = args)

      expect_identical(
        bridge$modules[[2]]$data,
        iris %>% filter(.data$Species == flower)
      )
    })
  }
})
