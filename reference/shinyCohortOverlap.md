# shinyCohortOverlap

shinyCohortOverlap

## Usage

``` r
shinyCohortOverlap(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  [`summariseCohortOverlap()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortOverlap.html)
  function from the `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`shiny.appojb`

## Examples

``` r
if (interactive()) {
  moduleCohortOverlap(result)
}
```
