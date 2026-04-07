# Characterisitcs Module Class

Characteristics module that shows characteristics results (table and
plot) from the CohortCharacteristics package.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Characteristics`

## Active bindings

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCharacteristics.html).

## Methods

### Public methods

- [`Characteristics$new()`](#method-Characteristics-new)

- [`Characteristics$clone()`](#method-Characteristics-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    Characteristics$new(result = NULL, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result of
  [`CohortCharacteristics::summariseCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCharacteristics.html).

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Characteristics$clone(deep = FALSE)

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
     "CohortCharacteristics",
     character.only = TRUE,
     quietly = TRUE,
     warn.conflicts = FALSE
   )
 ) {
    result <- omopgenerics::importSummarisedResult(system.file(
      package = "DarwinShinyModules",
      "dummyData/CohortCharacteristics/1.1.1/characteristics.csv"
    ))

    charMod <- Characteristics$new(result = result)

    ui <- shiny::fluidPage(
      charMod$UI()
    )

    server <- function(input, output, session) {
      charMod$server(input, output, session)
    }

    if (interactive()) {
      shiny::shinyApp(ui = ui, server = server)
    }
  }
# }
}
#> Reading file:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/CohortCharacteristics/1.1.1/characteristics.csv.
#> Converting to summarised_result:
#> /home/runner/work/_temp/Library/DarwinShinyModules/dummyData/CohortCharacteristics/1.1.1/characteristics.csv.
```
