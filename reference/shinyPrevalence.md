# shinyPrevalence

shinyPrevalence

## Usage

``` r
shinyPrevalence(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from either `estimatePeriodPrevalence()`
  or `estimatePointPrevalence()` function from the `IncidencePrevalence`
  pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`shiny.appobj`

## Examples

``` r
if (interactive()) {
  shinyPrevalence(result)
}
```
