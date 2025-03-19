test_that("DatabaseDBC", {
  skip_if_not_installed(c("DatabaseConnector", "SqlRender"))
  tempDir <- file.path(tempdir(), "loading_data_example")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = file.path(tempDir, "database.sqlite")
  )

  connection <- DatabaseConnector::connect(connectionDetails)

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "iris",
    data = iris
  )

  DatabaseConnector::disconnect(connection)

  db <- DatabaseDBC$new(connectionDetails)

  modServer <- function(id) {
    db$server(input, output, session)
  }

  testServer(modServer, {
    expect_true(db$connected)
    expect_equal(nrow(db$query("SELECT * FROM main.iris LIMIT 10")), 10)
    db$execute("DROP TABLE iris")
    expect_error(db$query("SELECT * FROM main.iris"))
  })
  expect_false(db$connected)
})
