# DatabaseDBC Class

This class extends the `Database` class, to query a database using
`DatabaseConnector` with JDBC.

## Super classes

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\>
[`DarwinShinyModules::Database`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Database.md)
-\> `DatabaseDBC`

## Methods

### Public methods

- [`DatabaseDBC$new()`](#method-DatabaseDBC-new)

- [`DatabaseDBC$query()`](#method-DatabaseDBC-query)

- [`DatabaseDBC$execute()`](#method-DatabaseDBC-execute)

- [`DatabaseDBC$uploadTable()`](#method-DatabaseDBC-uploadTable)

- [`DatabaseDBC$connect()`](#method-DatabaseDBC-connect)

- [`DatabaseDBC$disconnect()`](#method-DatabaseDBC-disconnect)

- [`DatabaseDBC$clone()`](#method-DatabaseDBC-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    DatabaseDBC$new(connectionDetails, ...)

#### Arguments

- `connectionDetails`:

  (`ConnectionDetails`) Connection Details from
  [`DatabaseConnector::createConnectionDetails`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`invisible(self)`

------------------------------------------------------------------------

### Method `query()`

Query method to query the database using
[`DatabaseConnector::renderTranslateQuerySql`](https://ohdsi.github.io/DatabaseConnector/reference/renderTranslateQuerySql.html)

#### Usage

    DatabaseDBC$query(sql, ...)

#### Arguments

- `sql`:

  (`character(1)`) SQL Query

- `...`:

  Additional parameters for
  [`DatabaseConnector::renderTranslateQuerySql`](https://ohdsi.github.io/DatabaseConnector/reference/renderTranslateQuerySql.html)

#### Returns

`data.frame`

------------------------------------------------------------------------

### Method `execute()`

Execute method to execute a query on the database using
[`DatabaseConnector::renderTranslateExecuteSql`](https://ohdsi.github.io/DatabaseConnector/reference/renderTranslateExecuteSql.html)

#### Usage

    DatabaseDBC$execute(sql, ...)

#### Arguments

- `sql`:

  (`character(1)`) SQL Query to execute

- `...`:

  Additional parameters for
  [`DatabaseConnector::renderTranslateExecuteSql`](https://ohdsi.github.io/DatabaseConnector/reference/renderTranslateExecuteSql.html)

#### Returns

`invisible(self)`

------------------------------------------------------------------------

### Method `uploadTable()`

Method to upload data to the database

#### Usage

    DatabaseDBC$uploadTable(tableName, data)

#### Arguments

- `tableName`:

  (`character(1)`) Name of the table

- `data`:

  (`data.frame`) data.frame like table to upload

------------------------------------------------------------------------

### Method `connect()`

Method to connect to the database.

#### Usage

    DatabaseDBC$connect()

------------------------------------------------------------------------

### Method `disconnect()`

Method to disconnect from the database.

#### Usage

    DatabaseDBC$disconnect()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatabaseDBC$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
