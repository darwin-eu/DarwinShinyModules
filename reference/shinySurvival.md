# shinySurvival

shinySurvival

## Usage

``` r
shinySurvival(result, .softValidation)
```

## Arguments

- result:

  (`summarised_result`) Result from either
  `estimateCompetingRiskSurvival()` or `estimateSingleEventSurvival()`
  functions from the CohortSurvival package.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`shiny.appobj`

## Examples

``` r
if (interactive()) {
  shinySurvival(result)
}
```
