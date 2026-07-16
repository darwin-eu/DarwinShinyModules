# Indication Module Class

Indication module that shows a that supports results from the
[`summariseIndication()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseIndication.html)
function from the DrugUtilisation package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Indication`

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

- [`Indication$new()`](#method-Indication-initialize)

- [`Indication$clone()`](#method-Indication-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `Indication$new()`

Initializer method.

#### Usage

    Indication$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Object created by
  [`DrugUtilisation::summariseDrugUtilisation()`](https://darwin-eu.github.io/DrugUtilisation/reference/summariseDrugUtilisation.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `Indication$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Indication$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (interactive()) {
  cdm <- DrugUtilisation::mockDrugUtilisation(numberIndividual = 100, source = "duckdb")

  cdm <- DrugUtilisation::generateIngredientCohortSet(
    cdm = cdm,
    name = "dus_cohort",
    ingredient = "acetaminophen",
    gapEra = 7
  )

  indications <- list(headache = 378253, influenza = 4266367)

  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = indications,
    name = "indications_cohort"
  )

  result <- cdm$dus_cohort |>
    PatientProfiles::addAge(
      ageGroup = list(
        `0-17` = c(0, 17),
        `>=18` = c(18, Inf)
      )
    ) |>
    PatientProfiles::addSex() |>
    DrugUtilisation::summariseIndication(
      indicationCohortName = "indications_cohort",
      unknownIndicationTable = "condition_occurrence",
      indicationWindow = list(c(-30, 0)),
      strata = list(
        "age_group",
        "sex"
      )
    )

  mod <- Indication$new(result)

  DarwinShinyModules::preview(mod)
}
```
