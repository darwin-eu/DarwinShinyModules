# moduleCohortCodelist

moduleCohortCodelist

## Usage

``` r
moduleCohortCodelist(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  [`summariseCohortCodelist()`](https://darwin-eu.github.io/CohortCharacteristics/reference/summariseCohortCodelist.html)
  function from the `CohortCharacteristics` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleCohortCodelist(result)
}
```
