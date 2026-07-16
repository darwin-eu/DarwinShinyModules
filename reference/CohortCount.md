# CohortCount Module Class

CohortCount module that shows characteristics results (table and plot)
from the CohortCharacteristics package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortCount`

## Active bindings

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortCount()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortCount.html).

## Methods

### Public methods

- [`CohortCount$new()`](#method-CohortCount-initialize)

- [`CohortCount$clone()`](#method-CohortCount-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `CohortCount$new()`

Initializer method

#### Usage

    CohortCount$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortCount()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortCount.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `CohortCount$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortCount$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
if (interactive()) {
  library(DarwinShinyModules)

  cdm <- CohortCharacteristics::mockCohortCharacteristics()

  result <- cdm$cohort1 |>
    PatientProfiles::addSex() |>
    CohortCharacteristics::summariseCohortCount(strata = "sex")

  mod <- CohortCount$new(result)

  preview(mod)
}
}
```
