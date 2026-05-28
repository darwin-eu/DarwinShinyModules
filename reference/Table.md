# Table Module Class

Table module that displays `data.frame` like objects as a table that are
supported by
[`DT::renderDT()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html) and
[`DT::DTOutput()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html).

## Details

The Table module exposes reactive bindings from the datatable object
from `DT`, in `bindings` field. These bindings are:

- cell_clicked

- cells_selected

- cell_info

- rows_current

- rows_all

- rows_selected

- row_last_clicked

- columns_selected

- search

- search_columns

- state

These bindings allow you to trigger events with i.e.
[`shiny::observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html)
in another module.

For a full description of the exposed bindings, consult the `DT`
documentation: https://rstudio.github.io/DT/shiny.html

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `Table`

## Active bindings

- `bindings`:

  (`reactiveValues`) Reactive bindings for
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html).

- `data`:

  (`data.frame`) Reactive data, use
  [`shiny::isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html) to
  get the non-reactive data.

- `title`:

  (`character`) Title of the table.

- `options`:

  (`list(n)`) List of options used by
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html).

- `filter`:

  (`character(1)`) Filter option used by
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html).

## Methods

### Public methods

- [`Table$new()`](#method-Table-initialize)

- [`Table$validate()`](#method-Table-validate)

- [`Table$clone()`](#method-Table-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)

------------------------------------------------------------------------

### `Table$new()`

initialize

#### Usage

    Table$new(
      data,
      title = NULL,
      options = list(scrollX = TRUE),
      filter = "top",
      ...
    )

#### Arguments

- `data`:

  (`data.frame`) Data to plot with, usually a `data.frame`-like object.

- `title`:

  (`character(1)`: `NULL`) Title of the table. When set to `NULL`, no
  title is shown.

- `options`:

  (`list`) table options, by default it shows additional items next to
  the table like search box, pagination, etc. Only display the table
  using list(dom = ”)

- `filter`:

  (`character`: `"top"`) filter option, it can be either `"none"`,
  `"bottom"` or `"top"` (default)

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `Table$validate()`

validate

#### Usage

    Table$validate()

#### Returns

`self`

------------------------------------------------------------------------

### `Table$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Table$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

table <- Table$new(data = mtcars)

if (interactive()) {
  preview(table)
}
```
