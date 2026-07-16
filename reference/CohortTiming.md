# CohortTiming Module Class

CohortTiming module that shows cohort timing results (table and plot)
from the CohortCharacteristics package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortTiming`

## Active bindings

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortTiming()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortTiming.html).

## Methods

### Public methods

- [`CohortTiming$new()`](#method-CohortTiming-initialize)

- [`CohortTiming$clone()`](#method-CohortTiming-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `CohortTiming$new()`

Initializer method

#### Usage

    CohortTiming$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCohortTiming()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortTiming.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `CohortTiming$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortTiming$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
if (interactive()) {
  library(CohortCharacteristics)
  library(omock)
  library(DrugUtilisation)

  cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")

  cdm <- generateIngredientCohortSet(
    cdm = cdm,
    name = "my_cohort",
    ingredient = c("acetaminophen", "morphine", "warfarin")
  )

  result <- summariseCohortTiming(cdm$my_cohort)

  mod <- CohortTiming$new(result)

  preview(mod)
}
}
```
