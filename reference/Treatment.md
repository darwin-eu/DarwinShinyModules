# Treatment Module Class

Restart module that shows a that supports results from the
`summariseTreatment()` function from the DrugUtilisation package.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Treatment`

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

- [`Treatment$new()`](#method-Treatment-new)

- [`Treatment$clone()`](#method-Treatment-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method.

#### Usage

    Treatment$new(result, ...)

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

    Treatment$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
  cdm <- DrugUtilisation::mockDrugUtilisation()

  result <- cdm$cohort1 |>
    PatientProfiles::addAge(
      ageGroup = list(
        `0-17` = c(0, 17),
        `>=18` = c(18, Inf)
      )
    ) |>
    PatientProfiles::addSex() |>
    DrugUtilisation::summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30), c(31, 365)), strata = list("age_group", "sex")
    )

  mod <- Treatment$new(result = result)

  DarwinShinyModules::preview(mod)
}
```
