# Preview

Launches a shiny app with the modules' `server()` and `UI()` methods.

## Usage

``` r
preview(modules)
```

## Arguments

- modules:

  ([ShinyModule](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md))
  A vector of module objects.

## Value

`NULL`

## Examples

``` r
table <- Table$new(data = iris)

if (interactive()) {
  preview(table)
}
```
