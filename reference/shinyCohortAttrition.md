# shinyCohortAttrition

shinyCohortAttrition

## Usage

``` r
shinyCohortAttrition(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `summariseCohortAttrition`
  function from the CohortCharacteristics pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`shiny.appobj`

## Examples

``` r
if (interactive()) {
  shinyCohortAttrition(result)
}
```
