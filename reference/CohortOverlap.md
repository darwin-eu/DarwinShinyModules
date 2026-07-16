# CohortOverlap Module Class

CohortOverlap module that shows cohort overlap results (table and plot)
from the CohortCharacteristics package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortOverlap`

## Active bindings

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortOverlap()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortOverlap.html).

## Methods

### Public methods

- [`CohortOverlap$new()`](#method-CohortOverlap-initialize)

- [`CohortOverlap$clone()`](#method-CohortOverlap-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `CohortOverlap$new()`

Initializer method

#### Usage

    CohortOverlap$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortOverlap()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortOverlap.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `CohortOverlap$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortOverlap$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
if (interactive()) {
  library(CohortCharacteristics)
  library(DarwinShinyModules)

  cdm <- mockCohortCharacteristics()

  result <- summariseCohortOverlap(cdm$cohort2)

  mod <- CohortOverlap$new(result)

  preview(mod)
}
}
```
