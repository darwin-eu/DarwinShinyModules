# moduleCharacteristics

moduleCharacteristics

## Usage

``` r
moduleCharacteristics(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  [`summariseCharacteristics()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCharacteristics.html)
  function from the `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleCharacteristics(result)
}
```
