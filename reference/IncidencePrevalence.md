# IncidencePrevalence Module Class

IncidencePrevalence module that shows incidence results from the
IncidencePrevalence package.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `IncidencePrevalence`

## Active bindings

- `result`:

  (`summarisedResult`) SummarisedResult object from IncidencePrevalence

- `pickers`:

  (`list`) List of pickers

## Methods

### Public methods

- [`IncidencePrevalence$new()`](#method-IncidencePrevalence-new)

- [`IncidencePrevalence$clone()`](#method-IncidencePrevalence-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    IncidencePrevalence$new(result, defaults = list(), ...)

#### Arguments

- `result`:

  (`summarised_result`) Result object from the `IncidencePrevalence`
  package.

- `defaults`:

  list of default values for the pickers

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

- `type`:

  (`character(1)`) Either `"incidence"` or `"prevalence"`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    IncidencePrevalence$clone(deep = FALSE)

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
    inc <- omopgenerics::importSummarisedResult(system.file(
      package = "DarwinShinyModules",
      "dummyData/IncidencePrevalence/1.2.0/incidence.csv"
    ))

    incMod <- IncidencePrevalence$new(result = inc,
                            defaults = list(sex = "Both"))

    ui <- shiny::fluidPage(
      incMod$UI()
    )

    server <- function(input, output, session) {
      incMod$server(input, output, session)
    }

    if (interactive()) {
      shiny::shinyApp(ui = ui, server = server)
    }
  }
# }
}
#> Reading file:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/IncidencePrevalence/1.2.0/incidence.csv.
#> Converting to summarised_result:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/IncidencePrevalence/1.2.0/incidence.csv.
```
