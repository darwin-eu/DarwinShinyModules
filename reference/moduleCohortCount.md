# moduleCohortCount

moduleCohortCount

## Usage

``` r
moduleCohortCount(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  [`summariseCohortCount()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortCount.html)
  function from the `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleCohortCount(result)
}
```
