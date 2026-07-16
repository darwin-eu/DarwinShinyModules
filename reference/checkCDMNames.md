# checkCDMNames

Checks the CDM names in a result. You can checkout the valid options
running:
[`getCDMAcronyms()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/getCDMAcronyms.md).

## Usage

``` r
checkCDMNames(result, .softValidation)
```

## Arguments

- result:

  (`result`) Result to check CDM names for.

- .softValidation:

  (`logical(1)`) If the check fails, throws a warning, rather than an
  error.

## Value

When the check passes returns `invisible(TRUE)`, otherwise returns the
error

## Examples

``` r
if (interactive()) {
  checkCDMNames(result)
}
```
