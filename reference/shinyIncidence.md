# shinyIncidence

shinyIncidence

## Usage

``` r
shinyIncidence(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `estimateIncidence()` function
  from the `IncidencePrevalence` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`shiny.appobj`

## Examples

``` r
if (interactive()) {
  shinyIncidence(result)
}
```
