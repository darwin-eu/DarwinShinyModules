# DoseCoverage Module Class

DoseCoverage module that shows a table

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DoseCoverage`

## Active bindings

- `result`:

  (`summarised_result`) Result

## Methods

### Public methods

- [`DoseCoverage$new()`](#method-DoseCoverage-initialize)

- [`DoseCoverage$clone()`](#method-DoseCoverage-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `DoseCoverage$new()`

Initializer method.

#### Usage

    DoseCoverage$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseDoseCoverage()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDoseCoverage.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `DoseCoverage$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DoseCoverage$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
if (interactive()) {
  library(DarwinShinyModules)
  library(DrugUtilisation)

  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividuals = 100)

  res <- summariseDoseCoverage(cdm = cdm, ingredientConceptId = 1125315)

  mod <- DoseCoverage$new(res)
  preview(mod)
}
}
```
