# CohortCodelist Module Class

CohortCodelist module that shows characteristics results (table) from
the CohortCharacteristics package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `CohortCodelist`

## Active bindings

- `result`:

  (`summarised_result`) Result object from
  `CohortCharacteristics::summariseCodelist()`.

## Methods

### Public methods

- [`CohortCodelist$new()`](#method-CohortCodelist-initialize)

- [`CohortCodelist$clone()`](#method-CohortCodelist-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `CohortCodelist$new()`

Initializer method

#### Usage

    CohortCodelist$new(result, ...)

#### Arguments

- `result`:

  (`summarised_result`) Result object from
  `CohortCharacteristics::summariseCodelist()`.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `CohortCodelist$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortCodelist$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
# \donttest{
if (interactive()) {
  library(DarwinShinyModules)

  result <- CohortCharacteristics::summariseCohortCodelist(cdm$my_cohort)

  mod <- CohortCodelist$new(result)

  preview(mod)
}
# }
}
```
