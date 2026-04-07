# PlotStatic Module Class

Static plot Module that handles static plots like from the
[`base::plot()`](https://rdrr.io/r/base/plot.html) function or `ggplot2`
objects.

## Super classes

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\>
[`DarwinShinyModules::Plot`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/Plot.md)
-\> `PlotStatic`

## Active bindings

- `plot`:

  Plot object.

## Methods

### Public methods

- [`PlotStatic$clone()`](#method-PlotStatic-clone)

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

    PlotStatic$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)
library(ggplot2)

staticFun <- function(data) {
  ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point() +
    theme_bw()
}

staticModule <- PlotStatic$new(fun = staticFun, args = list(data = iris))

if (interactive()) {
  preview(staticModule)
}
```
