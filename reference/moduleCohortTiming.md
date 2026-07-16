# moduleCohortTiming

moduleCohortTiming

## Usage

``` r
moduleCohortTiming(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  [`summariseCohortTiming()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortTiming.html)
  function from the `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleCohortTiming(result)
}
```
