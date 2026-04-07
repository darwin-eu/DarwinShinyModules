# PlotPlotly Module Class

Plotly module that handles `plotly` objects.

## Details

`Plotly` exposes bindings to interact with the plot programaticaly.
Currently, only the `plotly_selected` binding is supported in this
module.

## Super classes

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\>
[`DarwinShinyModules::Plot`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Plot.md)
-\> `PlotPlotly`

## Active bindings

- `plot`:

  (`plotly`) object.

- `source`:

  (`character`) Source label for the plotly plot.

- `bindings`:

  (`reactivevalues`) bindings from the plotly object.

## Methods

### Public methods

- [`PlotPlotly$clone()`](#method-PlotPlotly-clone)

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

    PlotPlotly$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

plotlyFun <- function(data) {
  plotly::ggplotly(
    ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_bw()
  )
}

plotlyModule <- PlotPlotly$new(fun = plotlyFun, args = list(data = iris))

if (interactive()) {
  preview(plotlyModule)
}
```
