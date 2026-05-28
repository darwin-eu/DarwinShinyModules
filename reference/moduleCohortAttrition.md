# moduleCohortAttrition

moduleCohortAttrition

## Usage

``` r
moduleCohortAttrition(result, .softValidation)
```

## Arguments

- result:

  (`summarised_result`) Result from the `summariseCohortAttrition`
  function from the CohortCharacteristics pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleCohortAttrition(result)
}
```
