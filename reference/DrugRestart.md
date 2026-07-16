# DrugRestart Module Class

DrugRestart module that shows a that supports results from the
[`summariseDrugRestart()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugRestart.html)
function from the DrugUtilisation package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DrugRestart`

## Active bindings

- `result`:

  (`sumamrised_result`)

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

## Methods

### Public methods

- [`DrugRestart$new()`](#method-DrugRestart-initialize)

- [`DrugRestart$clone()`](#method-DrugRestart-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `DrugRestart$new()`

Initializer method.

#### Usage

    DrugRestart$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseDrugUtilisation()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugUtilisation.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `DrugRestart$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DrugRestart$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
  cdm <- DrugUtilisation::mockDrugUtilisation()

  conceptlist <- list(
   acetaminophen = 1125360,
   metformin = c(1503297, 1503327)
  )

  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
   cdm = cdm,
   name = "switch_cohort",
   conceptSet = conceptlist
  )

  result <- cdm$cohort1 |>
   PatientProfiles::addAge(
     ageGroup = list(
       `0-17` = c(0, 17),
       `>=18` = c(18, Inf)
     )
   ) |>
   PatientProfiles::addSex() |>
   DrugUtilisation::summariseDrugRestart(
     switchCohortTable = "switch_cohort",
     strata = list("age_group", "sex")
   )

  mod <- DrugRestart$new(result)

  DarwinShinyModules::preview(mod)
}
```
