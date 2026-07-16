# moduleProportionOfPatientsCovered

moduleProportionOfPatientsCovered

## Usage

``` r
moduleProportionOfPatientsCovered(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the
  `summariseProportionOfPatientsCovered` function from the
  DrugUtilisation pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleProportionOfPatientsCovered(result)
}
```
