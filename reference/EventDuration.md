# EventDuration

Module that displays the Event Duration from the `TreatmentPatterns`
package.

## Super class

[`DarwinShinyModules::ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `EventDuration`

## Active bindings

- `plot`:

  (`PlotPlotly`) module.

- `table`:

  (`Table`) module.

- `inputPanel`:

  (`InputPanel`) module.

- `summaryEventDuration`:

  (`data.table`)

- `cdmSourceInfo`:

  (`data.frame`)

## Methods

### Public methods

- [`EventDuration$new()`](#method-EventDuration-new)

- [`EventDuration$clone()`](#method-EventDuration-clone)

Inherited methods

- [`DarwinShinyModules::ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`DarwinShinyModules::ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`DarwinShinyModules::ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`DarwinShinyModules::ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    EventDuration$new(summaryEventDuration, cdmSourceInfo, ...)

#### Arguments

- `summaryEventDuration`:

  (`data.frame`) `summary_event_duration` field from the
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

    EventDuration$clone(deep = FALSE)

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

    eventDuration <- EventDuration$new(
      summaryEventDuration = tpr$summary_event_duration,
      cdmSourceInfo = tpr$cdm_source_info
    )

    preview(eventDuration)
  }
}
```
