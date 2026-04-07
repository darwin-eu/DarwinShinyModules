# DatabaseDBI Class

This class extends the `Database` class, to query a database using `DBI`
with ODBC.

## Super classes

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\>
[`DarwinShinyModules::Database`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Database.md)
-\> `DatabaseDBI`

## Active bindings

- `connection`:

  (`DBI Connection`) A DBI connection.

- `driver`:

  (`driver`) Driver used to connect to the database.

- `connectArgs`:

  (`list(n)`) Named list of additional arguments used in
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- `disconnectArgs`:

  (`list(n)`) Named list of additional arguments used in
  [`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)

## Methods

### Public methods

- [`DatabaseDBI$new()`](#method-DatabaseDBI-new)

- [`DatabaseDBI$connect()`](#method-DatabaseDBI-connect)

- [`DatabaseDBI$disconnect()`](#method-DatabaseDBI-disconnect)

- [`DatabaseDBI$clone()`](#method-DatabaseDBI-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)
- [`DarwinShinyModules::Database$uploadTable()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Database.html#method-uploadTable)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    DatabaseDBI$new(driver, ...)

#### Arguments

- `driver`:

  Driver to use to connect to the database with
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`invisible(self)`

------------------------------------------------------------------------

### Method `connect()`

Method to connect to the database.

#### Usage

    DatabaseDBI$connect()

------------------------------------------------------------------------

### Method `disconnect()`

Method to disconnect from the database.

#### Usage

    DatabaseDBI$disconnect()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatabaseDBI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
