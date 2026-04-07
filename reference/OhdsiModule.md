# OhdsiModule Class

The OhdsiModule wraps around a `viewerX()` and `serverX()` function from
`OhdsiShinyModules`, running the module stand-alone. This setup should
also support any module from `OhdsiShinyModules`.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `OhdsiModule`

## Active bindings

- `connectionHandler`:

  (`ConnectionHandler`) ConnectionHandler object from
  `ResultModelManager`.

- `viewerFun`:

  (`function`) Viewer function to use from `OhdsiShinyModules`.

- `serverFun`:

  (`function`) Server function to use from `OhdsiShinyModules`.

- `resultDatabaseSettings`:

  (`list`) Named List of table prefixes like
  `ShinyAppBuilder::createDefaultResultDatabaseSettings()` creates.

## Methods

### Public methods

- [`OhdsiModule$new()`](#method-OhdsiModule-new)

- [`OhdsiModule$clone()`](#method-OhdsiModule-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    OhdsiModule$new(
      connectionHandler,
      viewerFun,
      serverFun,
      resultDatabaseSettings = ShinyAppBuilder::createDefaultResultDatabaseSettings(),
      ...
    )

#### Arguments

- `connectionHandler`:

  (`ConnectionHandler`) ConnectionHandler object from
  `ResultModelManager`.

- `viewerFun`:

  (`function`) Viewer function to use from `OhdsiShinyModules`.

- `serverFun`:

  (`function`) Server function to use from `OhdsiShinyModules`.

- `resultDatabaseSettings`:

  (`list`) Named List of table prefixes like
  `ShinyAppBuilder::createDefaultResultDatabaseSettings()` creates.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OhdsiModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# \donttest{
if (interactive()) {
  library(DarwinShinyModules)
  # library(DatabaseConnector)
  # library(ResultModelManager)
  # library(OhdsiShinyModules)
  # library(ShinyAppBuilder)

  connectionDetails <- createConnectionDetails(
    dbms = "sqlite",
    server = file.path(tempdir(), "results.sqlite")
  )

  connectionHandler <- ConnectionHandler$new(
    connectionDetails = connectionDetails
  )

  estimation <- OhdsiModule$new(
    connectionHandler = connectionHandler,
    viewerFun = estimationViewer,
    serverFun = estimationServer,
    resultDatabaseSettings = createDefaultResultDatabaseSettings()
  )

  preview(estimation)
}
# }
```
