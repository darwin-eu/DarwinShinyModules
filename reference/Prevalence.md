# Prevalence Module Class

Prevalence module that shows prevalence results from the
IncidencePrevalence package.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Prevalence`

## Active bindings

- `result`:

  (`summarisedResult`) SummarisedResult object from Prevalence.

- `pickers`:

  (`list`) List of pickers

## Methods

### Public methods

- [`Prevalence$new()`](#method-Prevalence-new)

- [`Prevalence$clone()`](#method-Prevalence-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    Prevalence$new(result, defaults, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result object from the `IncidencePrevalence`
  package.

- `defaults`:

  list of default values for the pickers

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Prevalence$clone(deep = FALSE)

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
     "IncidencePrevalence",
     character.only = TRUE,
     quietly = TRUE,
     warn.conflicts = FALSE
   )
 ) {
    prev <- omopgenerics::importSummarisedResult(system.file(
      package = "DarwinShinyModules",
      "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"
    ))

    prevMod <- Prevalence$new(result = prev,
                              defaults = list(sex = "Both"))

    ui <- shiny::fluidPage(
      prevMod$UI()
    )

    server <- function(input, output, session) {
      prevMod$server(input, output, session)
    }

    if (interactive()) {
      shiny::shinyApp(ui = ui, server = server)
    }
  }
# }
}
#> Reading file:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/IncidencePrevalence/1.2.0/prevalence.csv.
#> Converting to summarised_result:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/IncidencePrevalence/1.2.0/prevalence.csv.
```
