# saveAppStructure

Wrapper for
[`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html). Saves an
`appStructure` object as a qs-file using `qs2`.

## Usage

``` r
saveAppStructure(appStructure, filePath = "./appStructure.qs", ...)
```

## Arguments

- appStructure:

  (`list`: `NULL`) appStructure object, i.e. a named list of ShinyModule
  objects or (named) lists containing (named) ShinyModule objects.
  Representing the navigation (side) bar of the shiny app.

- filePath:

  (`character`: `"./appStructure.qs"`) Path to the qs-file, containing
  the `appStructure`, or where to write the `appStructure` to.

- ...:

  Extra arguments for
  [`qs2::qs_save()`](https://rdrr.io/pkg/qs2/man/qs_save.html)

## Value

`NULL` invisible
