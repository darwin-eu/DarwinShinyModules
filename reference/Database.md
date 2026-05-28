# Database Decorator Class

This class is a `decorator` and is not meant to be directly used, but to
be inherited by other modules, like `DatabaseDBC` and `DatabaseDBI`.

## Details

The inherited `Database` modules have their own implementation to
connect to, and query the database.

To add a new database type it is required to inherit from the `Database`
class, to override the private `connect()` and `disconnect()` methods,
and to extend it with functionality to query the database.

`DatabaseDBC` extends this class with the public `query()` and
`execute()` methods, while `DatabaseDBI` extends it with the
`attachTables()` and `detatchTables()` methods, and the public `tables`
field.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Database`

## Active bindings

- `connected`:

  (`logical(1)`) Logical if connected to the database.

## Methods

### Public methods

- [`Database$uploadTable()`](#method-Database-uploadTable)

- [`Database$connect()`](#method-Database-connect)

- [`Database$disconnect()`](#method-Database-disconnect)

- [`Database$clone()`](#method-Database-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$initialize()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-initialize)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `Database$uploadTable()`

Method to upload tables to the database.

#### Usage

    Database$uploadTable(tableName, data)

#### Arguments

- `tableName`:

  (`character(1)`) Name of the table

- `data`:

  (`data.frame`) data.frame like table to upload

------------------------------------------------------------------------

### `Database$connect()`

Method to connect to the database.

#### Usage

    Database$connect()

------------------------------------------------------------------------

### `Database$disconnect()`

Method to disconnect from the database.

#### Usage

    Database$disconnect()

------------------------------------------------------------------------

### `Database$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Database$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
