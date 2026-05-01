# DrugUtilisation Module Class

DrugUtilisation module that shows a that supports results from the
`summariseDrugutilisation()` function from the DrugUtilisation package.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DrugUtilisation`

## Active bindings

- `result`:

  (`summarised_result`)

- `table`:

  (`Flextable`)

- `plot`:

  (`PlotStatic`)

- `cdmNames`:

  (`character(n)`)

- `cohortNames`:

  (`character(n)`)

- `strata`:

  (`character(n)`)

- `variables`:

  (`character(n)`)

- `estimates`:

  (`character(n)`)

## Methods

### Public methods

- [`DrugUtilisation$new()`](#method-DrugUtilisation-new)

- [`DrugUtilisation$clone()`](#method-DrugUtilisation-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method.

#### Usage

    DrugUtilisation$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseDrugUtilisation()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugUtilisation.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DrugUtilisation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
 library(DarwinShinyModules)

 if (
   require(
     "DrugUtilisation",
     character.only = TRUE,
     quietly = TRUE,
     warn.conflicts = FALSE
   )
 ) {
    cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")

    cdm <- DrugUtilisation::generateIngredientCohortSet(
      cdm = cdm,
      name = "dus_cohort",
      ingredient = "acetaminophen",
      gapEra = 7
    )

    result <- cdm$dus_cohort |>
      PatientProfiles::addAge(ageGroup = list(
        `0-17` = c(0, 17),
        `>=18` = c(18, Inf)
      )) |>
      PatientProfiles::addSex() |>
      DrugUtilisation::summariseDrugUtilisation(
        ingredientConceptId = 1125315,
        gapEra = 7,
        strata = list("age_group", "sex")
      )

    duMod <- DrugUtilisation$new(result = result)
    preview(duMod)
  }
}
```
