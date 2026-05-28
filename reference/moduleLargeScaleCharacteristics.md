# moduleLargeScaleCharacteristics

moduleLargeScaleCharacteristics

## Usage

``` r
moduleLargeScaleCharacteristics(result, .softValidation)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  `summariseLargeScaleCharacteristics()` function from the
  `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleLargeScaleCharacteristics(result)
}
```
