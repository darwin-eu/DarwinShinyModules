# TreatmentPatterns Module Class

TreatmentPatterns module that shows results from the TreatmentPatterns
package.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `TreatmentPatterns`

## Active bindings

- `analyses`:

  Analyses table from TreatmentPatterns

- `treatment_pathways`:

  Analyses table from TreatmentPatterns

- `summary_event_duration`:

  Analyses table from TreatmentPatterns

- `counts_age`:

  Analyses table from TreatmentPatterns

- `counts_sex`:

  Analyses table from TreatmentPatterns

- `counts_year`:

  Analyses table from TreatmentPatterns

- `attrition`:

  Analyses table from TreatmentPatterns

- `metadata`:

  Analyses table from TreatmentPatterns

- `arguments`:

  Analyses table from TreatmentPatterns

- `cdm_source_info`:

  Analyses table from TreatmentPatterns

## Methods

### Public methods

- [`TreatmentPatterns$new()`](#method-TreatmentPatterns-initialize)

- [`TreatmentPatterns$clone()`](#method-TreatmentPatterns-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `TreatmentPatterns$new()`

Initializer method

#### Usage

    TreatmentPatterns$new(...)

#### Arguments

- `...`:

  Unnamed TreatmentPatternsResults objects. And Additional parameters to
  set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### `TreatmentPatterns$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TreatmentPatterns$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
