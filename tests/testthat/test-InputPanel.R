test_that("Creation", {
  inputPanel <- InputPanel$new(
    funs = list(
      picker = shinyWidgets::pickerInput,
      text = shiny::textInput
    ),
    args = list(
      picker = list(inputId = "select", choices = c("a", "b"), label = "select"),
      text = list(inputId = "text", label = "text")
    )
  )

  expect_equal(names(inputPanel$args), names(inputPanel$funs))

  expect_true(grepl(pattern = "text", inputPanel$args$text$inputId))
  expect_identical(inputPanel$args$text$label, "text")

  expect_identical(inputPanel$funs$picker, shinyWidgets::pickerInput)
  expect_identical(inputPanel$funs$text, shiny::textInput)
})

test_that("App", {
  inputPanel <- InputPanel$new(
    funs = list(
      picker = shinyWidgets::pickerInput,
      text = shiny::textInput
    ),
    args = list(
      picker = list(inputId = "select", choices = c("a", "b"), label = "select"),
      text = list(inputId = "text", label = "text")
    )
  )

  modServer <- function(id) {
    inputPanel$server(input, output, session)
  }

  testServer(modServer, {
    session$setInputs(
      text = "foo",
      picker = "a"
    )

    expect_identical(inputPanel$inputValues$text, input$text)
    expect_identical(inputPanel$inputValues$picker, input$picker)
  })
})
