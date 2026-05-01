# CohortSurvival Module Class

CohortSurvival module that shows a that supports results from the
CohortSurvival package.

## Details

The module consists of the following:

- "PlotPlotly":

  Interactive Plotly plot, visualizing the data.

- "GTTable":

  gttable visualizing the tidy data

- "InputPanel":

  Input panel dealing with user input

- "Table":

  basic table visualizing the raw data

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortSurvival`

## Active bindings

- `result`:

  (`SummarisedResult`) Summarised result object from `CohortSurvival`

- `table`:

  (`Table`) Table module using
  [`CohortSurvival::tableSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/tableSurvival.html)

- `tableEvents`:

  (`Table`) Table module using
  [`CohortSurvival::tableSurvivalEvents()`](https://darwin-eu.github.io/CohortSurvival/reference/tableSurvivalEvents.html)

- `plot`:

  (`Plot`) Plot module. using
  [`CohortSurvival::plotSurvival()`](https://darwin-eu.github.io/CohortSurvival/reference/plotSurvival.html)

- `tableAttrition`:

  (`Table`) Table module using `CohortSurvival::tableAttrition()`

- `cdmNames`:

  (`character`) Available CDM names

- `cohortNames`:

  (`character`) Available cohort bames

- `strata`:

  (`character`) Available strata names

## Methods

### Public methods

- [`CohortSurvival$new()`](#method-CohortSurvival-new)

- [`CohortSurvival$clone()`](#method-CohortSurvival-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer function

#### Usage

    CohortSurvival$new(result, ...)

#### Arguments

- `result`:

  (`SummarisedResults`) Summarised result object from `CohortSurvival`

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`invisible(self)`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortSurvival$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
# \donttest{
 library(DarwinShinyModules)

 if (
   require(
     "CohortSurvival",
     character.only = TRUE,
     quietly = TRUE,
     warn.conflicts = FALSE
   )
 ) {
    cdm <- CohortSurvival::mockMGUS2cdm()

    result <- CohortSurvival::estimateSingleEventSurvival(
      cdm,
      targetCohortTable = "mgus_diagnosis",
      outcomeCohortTable = "death_cohort",
      strata = list(
        c("age_group"),
        c("sex"),
        c("age_group", "sex")
      )
    )

    survMod <- DarwinShinyModules::CohortSurvival$new(result = result)
    if (interactive()) {
      DarwinShinyModules::preview(survMod)
    }
  }
# }
}
#> Creating a new cdm
#> Uploading table person (1384 rows) - [1/7]
#> Uploading table observation_period (1384 rows) - [2/7]
#> Uploading table visit_occurrence (1 rows) - [3/7]
#> Uploading table death_cohort (963 rows) - [4/7]
#> Uploading table mgus_diagnosis (1384 rows) - [5/7]
#> Uploading table progression (115 rows) - [6/7]
#> Uploading table progression_type (230 rows) - [7/7]
#> - Getting survival for target cohort 'mgus_diagnosis' and outcome cohort
#> 'death_cohort'
#> Getting overall estimates
#> `eventgap`, `outcome_washout`, `censor_on_cohort_exit`, `follow_up_days`, and
#> `minimum_survival_days` casted to character.
```
