test_that("ShinyModule namespacing", {
  mod <- ShinyModule$new()

  ## Base ----
  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleName,
      mod$instanceId
    ), collapse = "-")
  )

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleId
    ), collapse = "-")
  )

  ## Override parentNamespace ----
  mod$parentNamespace <- "parent"

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleName,
      mod$instanceId
    ), collapse = "-")
  )

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleId
    ), collapse = "-")
  )

  mod$parentNamespace <- NULL

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleName,
      mod$instanceId
    ), collapse = "-")
  )

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleId
    ), collapse = "-")
  )

  ## Override parentNamesapce, and instanceID ----
  mod$parentNamespace <- "parent"
  mod$instanceId <- "A"

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleName,
      mod$instanceId
    ), collapse = "-")
  )

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleId
    ), collapse = "-")
  )

  mod$parentNamespace <- NULL

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleName,
      mod$instanceId
    ), collapse = "-")
  )

  expect_identical(
    mod$namespace,
    paste(c(
      mod$parentNamespace,
      mod$moduleId
    ), collapse = "-")
  )

  rm(mod)
})

test_that("Overrides", {
  ## Override public server ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,
    public = list(
      server = function(input, output, session) {
        message("foo")
      }
    )
  )

  expect_error(MyMod$new())

  ## Override public UI ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,
    public = list(
      UI = function() {
        shiny::tagList()
      }
    )
  )

  expect_error(MyMod$new())

  ## Override public server and UI ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,
    public = list(
      UI = function() {
        shiny::tagList()
      },

      server = function(input, output, session) {
        message("foo")
      }
    )
  )

  expect_error(MyMod$new())

  ## Override public UI private server ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,
    public = list(
      UI = function() {
        shiny::tagList()
      }
    ),

    private = list(
      .server = function(input, output, session) {
        message("foo")
      }
    )
  )

  expect_error(MyMod$new())

  ## Override public server, private UI ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,
    public = list(
      server = function(input, output, session) {
        message("foo")
      }
    ),

    private = list(
      .UI = function() {
        shiny::tagList()
      }
    )
  )

  expect_error(MyMod$new())

  ## Override private server and UI ----
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,

    private = list(
      .UI = function() {
        shiny::tagList()
      },

      .server = function(input, output, session) {
        message("foo")
      }
    )
  )

  expect_true(all(class(MyMod$new()) %in% c("MyMod", "ShinyModule", "R6")))

  rm(MyMod)
})

test_that("app", {
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,

    private = list(
      .UI = function() {
        shiny::tableOutput(outputId = "table")
        shiny::selectInput(
          inputId = shiny::NS(mod$namespace, "select"),
          label = "F",
          choices = c("foo", "bar", "baz"),
          selected = "foo"
        )
      },

      .server = function(input, output, session) {
        output$table <- shiny::renderTable(iris)
        private$.reactiveValues$select <- ""
        shiny::observeEvent(input$select, {
          private$.reactiveValues$select <- input$select
        })
        private$.reactiveValues$x <- 3
      }
    )
  )

  mod <- MyMod$new()

  modServer <- function(id) {
    mod$server(input, output, session)
  }

  testServer(mod$server, {
    session$setInputs(select = "foo")
    print(mod$reactiveValues$select)
    print(input$select)
  })
})
