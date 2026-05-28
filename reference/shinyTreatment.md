# shinyTreatment

shinyTreatment

## Usage

``` r
shinyTreatment(result, .softValidation = FALSE)
```

## Arguments

- result:

  (`summarised_result`) Result from the `summariseTreatment` function
  from the DrugUtilisation pacakge.

- .softValidation:

  (`logical(1)`: `FALSE`) When `TRUE` will throw the failed check as a
  warning.

## Value

`ShinyModule`

## Examples

``` r
if (interactive()) {
  shinyTreatment(result)
}
```
