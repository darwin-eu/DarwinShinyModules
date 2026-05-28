# moduleTreatmentPatterns

Wrapper function to create a TreatmentPatterns module instance.

## Usage

``` r
moduleTreatmentPatterns(..., .softValidation = FALSE)
```

## Arguments

- ...:

  Unnamed result objects from the `export()` function from the
  TreatmentPatterns package.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`TreatmentPatterns` ShinyModule

## Examples

``` r
if (interactive()) {
  moduleTreatmentPatterns(tpr1, tpr2, tpr3)
}
```
