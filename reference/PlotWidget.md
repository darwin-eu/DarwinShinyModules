# PlotWidget Module Class

Widget module that handles `htmlwidget` objects.

## Super classes

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\>
[`DarwinShinyModules::Plot`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Plot.md)
-\> `PlotWidget`

## Methods

### Public methods

- [`PlotWidget$clone()`](#method-PlotWidget-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::Plot$initialize()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Plot.html#method-initialize)
- [`DarwinShinyModules::Plot$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Plot.html#method-validate)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotWidget$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

nD3Installed <- require(
"networkD3",
character.only = TRUE,
quietly = TRUE,
warn.conflicts = FALSE
)

if (nD3Installed) {
  src <- c(
    "A", "A", "A", "A",
    "B", "B", "C", "C", "D"
  )
  target <- c(
    "B", "C", "D", "J",
    "E", "F", "G", "H", "I"
  )

  widgetModule <- PlotWidget$new(fun = simpleNetwork, args = list(Data = data.frame(src, target)))

  if (interactive()) {
    preview(widgetModule)
  }
}
```
