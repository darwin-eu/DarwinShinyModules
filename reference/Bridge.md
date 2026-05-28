# Bridge Module Class

Bridge module that "birdges" multiple modules with bespoke server code.

## Details

The Bridge module links two or more modules together with user defined
server code.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Bridge`

## Active bindings

- `modules`:

  (`list`) List of modules.

- `birdgeFun`:

  (`function`) Function that bridges modules.

## Methods

### Public methods

- [`Bridge$new()`](#method-Bridge-initialize)

- [`Bridge$clone()`](#method-Bridge-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `Bridge$new()`

Initializer method.

#### Usage

    Bridge$new(..., bridgeFun = NULL)

#### Arguments

- `...`:

  (`ShinyModule`) ShinyModules to bridge.

- `bridgeFun`:

  (`function`: `NULL`) Server function to make the modules interact with
  eachother. Should be setup as a shiny server function that takes
  `input`, `output`, and `session` as parameters.

#### Returns

`self`

------------------------------------------------------------------------

### `Bridge$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Bridge$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

table <- Table$new(data = mtcars)

inputPanel <- InputPanel$new(
  funs = list(
    inputSpecies = shiny::selectInput
  ),
  args = list(
    inputSpecies = list(
      inputId = "inputSpecies",
      label = "Select Species",
      choices = unique(iris$Species),
      selected = unique(iris$Species)[1]
    )
  )
)

bridgeFun <- function(input, output, session) {
  shiny::observeEvent(inputPanel$inputValues$inputSpecies, {
    table$data <- iris %>%
      dplyr::filter(.data$Species == inputPanel$inputValues$inputSpecies)
  })
}

bridge <- Bridge$new(inputPanel, table, bridgeFun = bridgeFun)

if (interactive()) {
  preview(bridge)
}
```
