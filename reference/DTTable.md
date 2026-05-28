# DTTable Module Class

GTTable module that displays tables using `DT` that are supported by
[`DT::renderDT()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html) and
[`DT::DTOutput()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html).

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DTTable`

## Active bindings

- `fun`:

  (`function`) Function to produce a `gt` table with, i.e
  [`gt::gt`](https://gt.rstudio.com/reference/gt.html).

- `args`:

  (`list`) Arguments for said function as a named list i.e.
  `list(data = iris)`.

## Methods

### Public methods

- [`DTTable$new()`](#method-DTTable-initialize)

- [`DTTable$clone()`](#method-DTTable-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `DTTable$new()`

Initializer method.

#### Usage

    DTTable$new(fun, args, ...)

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

### `DTTable$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DTTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

dtTable <- DTTable$new(
  fun = DT::datatable,
  args = list(data = iris)
)

if (interactive()) {
  preview(dtTable)
}
```
