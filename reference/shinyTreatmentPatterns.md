# shinyTreatmentPatterns

shinyTreatmentPatterns

## Usage

``` r
shinyTreatmentPatterns(..., .softValidation = FALSE)
```

## Arguments

- ...:

  Unnamed result objects from the `export()` function from the
  TreatmentPatterns package.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

shiny.appobj

## Examples

``` r
if (interactive()) {
  shinyTreatmentPatterns(tpr1, tpr2, tpr3)
}
```
