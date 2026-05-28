# GTTable Module Class

GTTable module that displays tables using `gt` that are supported by
[`gt::render_gt()`](https://gt.rstudio.com/reference/render_gt.html) and
[`gt::gt_output()`](https://gt.rstudio.com/reference/gt_output.html).

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `GTTable`

## Active bindings

- `fun`:

  (`function`) Function to produce a `gt` table with, i.e
  [`gt::gt`](https://gt.rstudio.com/reference/gt.html).

- `args`:

  (`list`) Arguments for said function as a named list i.e.
  `list(data = iris)`.

## Methods

### Public methods

- [`GTTable$new()`](#method-GTTable-initialize)

- [`GTTable$clone()`](#method-GTTable-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `GTTable$new()`

Initializer method.

#### Usage

    GTTable$new(fun, args, ...)

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

### `GTTable$clone()`

The objects of this class are cloneable with this method.

#### Usage

    GTTable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

gtTable <- GTTable$new(
  fun = gt::gt,
  args = list(data = iris)
)

if (interactive()) {
  preview(gtTable)
}
```
