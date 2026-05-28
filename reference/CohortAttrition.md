# CohortAttrition Module Class

CohortAttrition module that displays tables and plots of the
`summarised_result` object created by
[`CohortCharacteristics::summariseCohortAttrition()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortAttrition.html).

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortAttrition`

## Active bindings

- `result`:

  (`summarised_result`)

- `cdmNames`:

  (`character(n)`)

- `cohortNames`:

  (`character(n)`)

## Methods

### Public methods

- [`CohortAttrition$new()`](#method-CohortAttrition-initialize)

- [`CohortAttrition$clone()`](#method-CohortAttrition-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `CohortAttrition$new()`

Initializer method.

#### Usage

    CohortAttrition$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`CohortCharacteristics::summariseLargeScaleCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseLargeScaleCharacteristics.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `CohortAttrition$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortAttrition$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
  CDMConnector::requireEunomia()
  con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main"
  )

  cdm <- DrugUtilisation::generateIngredientCohortSet(
    cdm = cdm,
    name = "my_cohort",
    ingredient = c("warfarin", "acetaminophen")
  )

  summarisedAttrition <- CohortCharacteristics::summariseCohortAttrition(cdm$my_cohort)

  mod <- Attrition$new(result = summarisedAttrition)
  DarwinShinyModules::preview(mod)
}
```
