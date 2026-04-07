# StudyBackground

StudyBackground Module that contains background information and the
EUPAS.

## Value

`invisible(self)`

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `StudyBackground`

## Active bindings

- `background`:

  (`character(n)`) Either the direct background, or the contents of a
  markdown (.md) file.

- `EUPAS`:

  (`character(1)`) EUPAS belonging to the study.

- `text`:

  (`Text`) A Text module.

## Methods

### Public methods

- [`StudyBackground$new()`](#method-StudyBackground-new)

- [`StudyBackground$clone()`](#method-StudyBackground-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

initializer method

#### Usage

    StudyBackground$new(background, EUPAS, ...)

#### Arguments

- `background`:

  (`character(n)`) Either a direct background description or a file path
  pointing to a markdown (.md) file.

- `EUPAS`:

  (`character(1)`) EUPAS belonging to the study.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StudyBackground$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

studyBackground <- StudyBackground$new(
  background = "./background.md",
  EUPAS = "EUPAS9999999"
)

if (interactive()) {
  preview(studyBackground)
}
```
