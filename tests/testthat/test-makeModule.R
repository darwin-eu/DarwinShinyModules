test_that("Creation", {
  ui <- shiny::tagList(p("My UI"))
  server <- function(input, output, session) {
    # Do stuff
  }

  mod <- makeModule(ui, server)
  testthat::expect_true(
    all(c("GenericModule", "ShinyModule", "R6") == class(mod))
  )

  expect_identical(mod$varServer, server)
  expect_identical(mod$varUI, ui)
})

test_that("App", {
  ui <- shiny::tagList(p("My UI"))
  server <- function(input, output, session) {
    # Do stuff
  }

  mod <- makeModule(ui, server)

  modServer <- function(id) {
    mod$server(input, output, session)
  }

  testServer(modServer, {
    suppressWarnings({
      expect_true(TRUE)
    })
  })
})
