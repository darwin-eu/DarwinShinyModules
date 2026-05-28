# ReactableTable Module Class

ReactableTable module that displays tables using `reactable` that are
supported by
[`reactable::renderReactable()`](https://glin.github.io/reactable/reference/reactable-shiny.html)
and
[`reactable::reactableOutput()`](https://glin.github.io/reactable/reference/reactable-shiny.html).

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `ReactableTable`

## Active bindings

- `fun`:

  (`function`) Function to produce a `gt` table with, i.e
  [`gt::gt`](https://gt.rstudio.com/reference/gt.html).

- `args`:

  (`list`) Arguments for said function as a named list i.e.
  `list(data = iris)`.

## Methods

### Public methods

- [`ReactableTable$new()`](#method-ReactableTable-initialize)

- [`ReactableTable$clone()`](#method-ReactableTable-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `ReactableTable$new()`

Initializer method.

#### Usage

    ReactableTable$new(fun, args, ...)

#### Arguments

- `fun`:

  (`function`) Function to produce a `gt` table with, i.e
  [`gt::gt`](https://gt.rstudio.com/reference/gt.html).

- `args`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Arguments for said
  function as a named list i.e. `list(data = iris)`.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `ReactableTable$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ReactableTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

reactableTable <- ReactableTable$new(
  fun = reactable::reactable,
  args = list(data = iris)
)

if (interactive()) {
  preview(reactableTable)
}
```
