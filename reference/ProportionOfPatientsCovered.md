# ProportionOfPatientsCovered Module Class

ProportionOfPatientsCovered module that shows tables and plots

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `ProportionOfPatientsCovered`

## Active bindings

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseProportionOfPatientsCovered()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseProportionOfPatientsCovered.html).

- `table`:

  (`Flextable`) ShinyModule

- `plot`:

  (`PlotStatic`) ShinyModule

## Methods

### Public methods

- [`ProportionOfPatientsCovered$new()`](#method-ProportionOfPatientsCovered-initialize)

- [`ProportionOfPatientsCovered$clone()`](#method-ProportionOfPatientsCovered-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `ProportionOfPatientsCovered$new()`

Initializer method.

#### Usage

    ProportionOfPatientsCovered$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseProportionOfPatientsCovered()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseProportionOfPatientsCovered.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `ProportionOfPatientsCovered$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProportionOfPatientsCovered$clone(deep = FALSE)

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

  res <- cdm$cohort1 |>
    DrugUtilisation::summariseProportionOfPatientsCovered(followUpDays = 365)

  mod <- ProportionOfPatientsCovered$new(res)
  preview(mod)
}
}
```
