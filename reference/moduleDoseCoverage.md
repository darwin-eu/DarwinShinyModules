# moduleDoseCoverage

moduleDoseCoverage

## Usage

``` r
moduleDoseCoverage(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `summariseDoseCoverage` function
  from the DrugUtilisation pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleDoseCoverage(result)
}
```
