test_that("Creation", {
  mod <- ShinyModule$new()
  expect_identical(class(mod), c("ShinyModule", "R6"))

  expect_identical(mod$moduleName, class(mod)[1])
  expect_true(is.character(mod$instanceId))
  expect_true(grepl(pattern = mod$moduleName, x = mod$moduleId))
  expect_true(grepl(pattern = mod$instanceId, x = mod$moduleId))
  expect_true(grepl(pattern = mod$moduleId, x = mod$namespace))
})

test_that("Namespacing", {
  mod <- ShinyModule$new()

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

test_that("Method Overrides", {
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

test_that("Decorated", {
  MyMod <- R6::R6Class(
    classname = "MyMod",
    inherit = ShinyModule,

    private = list(
      .server = function(input, output, session) {
        shiny::observeEvent(input$foo, {
          private$.reactiveValues$foo <- input$foo
        })
      }
    )
  )

  mod <- MyMod$new()

  modServer <- function(id) {
    mod$server(input, output, session)
  }

  testServer(modServer, {
    expect_true(is.reactivevalues(mod$reactiveValues))

    session$setInputs(foo = "bar")
    expect_equal(input$select, mod$reactiveValues$select)

    session$setInputs(foo = "baz")
    expect_equal(input$select, mod$reactiveValues$select)
  })
})
