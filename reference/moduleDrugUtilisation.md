# moduleDrugUtilisation

moduleDrugUtilisation

## Usage

``` r
moduleDrugUtilisation(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `summariseDrugUtilisation`
  function from the DrugUtilisation pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  moduleDrugUtilisation(result)
}
```
