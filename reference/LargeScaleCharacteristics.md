# LargeScaleCharacteristics Module Class

LargeScaleCharacteristics module that displays tables and plots of the
`summarised_result` object created by
[`CohortCharacteristics::summariseLargeScaleCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseLargeScaleCharacteristics.html).

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `LargeScaleCharacteristics`

## Active bindings

- `result`:

  (`summarised_result`)

- `tidyResult`:

  (`tbl_df`)

- `cdmNames`:

  (`character(n)`)

- `cohortNames`:

  (`character(n)`)

- `strata`:

  (`character(n)`)

- `windows`:

  (`character(n)`)

- `table`:

  (`ShinyModule`)

- `tableTop`:

  (`ShinyModule`)

- `plot`:

  (`ShinyModule`)

- `plotCompared`:

  (`ShinyModule`)

## Methods

### Public methods

- [`LargeScaleCharacteristics$new()`](#method-LargeScaleCharacteristics-new)

- [`LargeScaleCharacteristics$clone()`](#method-LargeScaleCharacteristics-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method.

#### Usage

    LargeScaleCharacteristics$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`CohortCharacteristics::summariseLargeScaleCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseLargeScaleCharacteristics.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LargeScaleCharacteristics$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
  CDMConnector::requireEunomia()
   con <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomiaDir())
   cdm <- CDMConnector::cdmFromCon(con = con, cdmSchema = "main", writeSchema = "main")

   cdm <- DrugUtilisation::generateIngredientCohortSet(
     cdm = cdm,
     name = "my_cohort",
     ingredient = c("warfarin", "acetaminophen")
  )

   cdm$my_cohort <- cdm$my_cohort |>
     PatientProfiles::addAge(ageGroup = list(
      `0 to 17` = c(0, 17),
      `>=18` = c(18, Inf)
    )) |>
    PatientProfiles::addSex()

  result <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$my_cohort,
    eventInWindow = "condition_occurrence",
    strata = list("age_group", "sex")
  )

  lscMod <- LargeScaleCharacteristics$new(result = result)
  DarwinShinyModules::preview(lscMod)
}
```
