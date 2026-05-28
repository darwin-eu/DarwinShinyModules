# moduleSurvival

Wrapper function to create a CohortSurvival module instance.

## Usage

``` r
moduleSurvival(result, .softValidation)
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

`TreatmentPatterns` ShinyModule

## Examples

``` r
if (interactive()) {
  moduleSurvival(tpr)
}
```
