# TreatmentPathways

Module that displays the Treatment Pathways from the `TreatmentPatterns`
package.

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `TreatmentPathways`

## Active bindings

- `colours`:

  (`list`) Hex colour values used in the Sunburst Plots and Sankey
  Diagrams.

- `sunburst`:

  (`PlotWidget`) Module.

- `sankey`:

  (`PlotWidget`) Module.

- `inputPanel`:

  (`InputPanel`) Module.

- `table`:

  Table displaying the `treatment_pathways` csv-file.

- `sunburstOverview`:

  (`list`) Containing Sunburst `PlotWidget` modules.

- `treatmentPathways`:

  (`data.frame`)

- `cdmSourceInfo`:

  (`data.frame`)

## Methods

### Public methods

- [`TreatmentPathways$new()`](#method-TreatmentPathways-new)

- [`TreatmentPathways$clone()`](#method-TreatmentPathways-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    TreatmentPathways$new(treatmentPathways, cdmSourceInfo, ...)

#### Arguments

- `treatmentPathways`:

  (`data.frame`) `treatment_pathways` field from the
  `TreatmentPatternsResult` object.

- `cdmSourceInfo`:

  (`data.frame`) `cdm_source_info` field from the
  `TreatmentPatternsResult` object.

- `...`:

  Additional parameters to set fields from the `ShinyModule` parent.

#### Returns

`self`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TreatmentPathways$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
{
  if (interactive()) {
    library(DarwinShinyModules)

    tpr <- TreatmentPatterns::TreatmentPatternsResults$new(
      filePath = system.file(package = "DarwinShinyModules", "dummyData/TreatmentPatterns/3.0.0/")
    )

    treatmentPathways <- TreatmentPathways$new(
      treatmentPathways = tpr$treatment_pathways,
      cdmSourceInfo = tpr$cdm_source_info
    )

    preview(treatmentPathways)
  }
}
```
