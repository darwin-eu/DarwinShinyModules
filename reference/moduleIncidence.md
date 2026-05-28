# moduleIncidence

moduleIncidence

## Usage

``` r
moduleIncidence(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `estimateIncidence()` function
  from the `IncidencePrevalence` pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleIncidence(result)
}
```
