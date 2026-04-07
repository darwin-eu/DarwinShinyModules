# Flextable Module Class

Flextable module that displays tables using `flextable` that are
displayed by `renderUI` and `uiOutput`.

## Value

`self`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Flextable`

## Active bindings

- `fun`:

  (`function`) Function to produce a `flextable` table with, i.e
  [`flextable::flextable`](https://davidgohel.github.io/flextable/reference/flextable.html).

- `args`:

  (`list`) Arguments for said function as a named list i.e.
  `list(data = iris)`.

## Methods

### Public methods

- [`Flextable$new()`](#method-Flextable-new)

- [`Flextable$clone()`](#method-Flextable-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method.

#### Usage

    Flextable$new(fun, args, ...)

#### Arguments

- `fun`:

  (`function`) Function to produce a `flextable` table with, i.e
  [`flextable::flextable`](https://davidgohel.github.io/flextable/reference/flextable.html).

- `args`:

  ([`list()`](https://rdrr.io/r/base/list.html)) Arguments for said
  function as a named list i.e. `list(data = iris)`.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Flextable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

gtTable <- Flextable$new(
  fun = flextable::flextable,
  args = list(data = iris)
)

if (interactive()) {
  preview(gtTable)
}
```
