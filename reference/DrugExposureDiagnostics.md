# DrugExposureDiagnostics Module Class

DrugExposureDiagnostics module that shows tables and plots

## Details

The module consists of the following:

- "dataPlotPanel":

  Table and a plot (bar or box) for each check.

- "metaDataPanel":

  Table containing the metadata.

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DrugExposureDiagnostics`

## Methods

### Public methods

- [`DrugExposureDiagnostics$new()`](#method-DrugExposureDiagnostics-new)

- [`DrugExposureDiagnostics$clone()`](#method-DrugExposureDiagnostics-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    DrugExposureDiagnostics$new(resultList, database_id = NULL, ...)

#### Arguments

- `resultList`:

  (`list`) List containing the output of the checks

- `database_id`:

  (`character`) Database identifier (optional)

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

(`invisible(self)`)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DrugExposureDiagnostics$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
library(DarwinShinyModules)

if (require("DrugExposureDiagnostics", character.only = TRUE, quietly = TRUE,
warn.conflicts = FALSE)) {
  ded <- readRDS(system.file(package = "DarwinShinyModules",
                             "dummyData/DrugExposureDiagnostics/1.1.1/ded.rds"))
  mod <- DrugExposureDiagnostics$new(resultList = ded)
  if (interactive()) {
    preview(mod)
  }
}
}
```
