test_that("DatabaseDBI", {
  skip_if_not_installed(c("duckdb", "DBI", "dbplyr"))
  tempDir <- file.path(tempdir(), "loading_data_example")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)

  driver <- duckdb::duckdb(
    dbdir = file.path(tempDir, "database.duckdb")
  )

  con <- DBI::dbConnect(
    drv = driver
  )

  DBI::dbWriteTable(
    conn = con,
    name = "iris",
    value = iris,
    overwrite = TRUE
  )

  DBI::dbDisconnect(con)

  db <- DatabaseDBI$new(driver)

  modServer <- function(id) {
    db$server(input, output, session)
  }

  testServer(modServer, {
    expect_true(db$connected)
    db$attachTables("iris")
    expect_equal(
      db$tables$iris |>
        dplyr::collect() |>
        head(n = 10) |>
        nrow(),
      10
    )
    db$detatchTables("iris")
    expect_null(db$tables$iris)
  })
  expect_false(db$connected)
})
