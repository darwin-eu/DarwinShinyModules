test_that("Creation", {
  table <- Table$new(
    data = iris,
    title = "Iris",
    options = list(scrollx = FALSE, scrolly = TRUE),
    filter = "bottom"
  )

  expect_identical(class(table), c("Table", "ShinyModule", "R6"))

  expect_true(is.null(isolate(table$reactiveValues$data)))
  expect_identical(table$data, iris)
  expect_identical(table$title, "Iris")
  expect_identical(table$options, list(scrollx = FALSE, scrolly = TRUE))
  expect_identical(table$filter, "bottom")
})

test_that("App", {
  table <- Table$new(data = iris)

  modServer <- function(id) {
    table$server(input, output, session)
  }

  testServer(modServer, {
    ## ReactiveValues ----
    expect_identical(
      isolate(table$reactiveValues$data),
      iris
    )

    ## Outputs ----
    expect_true(!is.null(output$table))
    expect_true(!is.null(output$dlButton))

    ## Bindings ----
    session$setInputs(
      table_rows_current = 1:3,
      table_cell_clicked = 2:4,
      table_cell_info = "information",
      table_cells_selected = 3:5,
      table_columns_selected = 4:6,
      table_row_last_clicked = 7,
      table_rows_all = 8:11,
      table_rows_selected = 12:15,
      table_search = "search term",
      table_search_columns = "search col",
      table_state = "status"
    )

    expect_identical(table$bindings$rows_current, 1:3)
    expect_identical(table$bindings$cell_clicked, 2:4)
    expect_identical(table$bindings$cell_info, "information")
    expect_identical(table$bindings$cells_selected, 3:5)
    expect_identical(table$bindings$columns_selected, 4:6)
    expect_identical(table$bindings$row_last_clicked, 7)
    expect_identical(table$bindings$rows_all, 8:11)
    expect_identical(table$bindings$rows_selected, 12:15)
    expect_identical(table$bindings$search, "search term")
    expect_identical(table$bindings$search_columns, "search col")
    expect_identical(table$bindings$state, "status")
  })
})
