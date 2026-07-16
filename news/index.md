# Changelog

## DarwinShinyModules 0.7.0

### Modules

- Made result/data fields writeable for composed modules
- Added `data` argument to
  [`makeModule()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/makeModule.md)

#### DrugUtilisation:

- Added `DoseCoverage`
- Added `ProportionOfPatientsCovered`

#### CohortCharacteristics:

- Added `CohortCount`
- Added `CohortTiming`
- Added `CohortOverlap`

### Shiny App Utils

- Updated footer
- Updated Data Partner acronyms
- Added utility functions to save, load, and deploy a shiny app directly
  from or to an `appStructure`.

### Dependencies

- Added `visOmopResults` to Imports
- Added `omock` to Suggests

### Documentation

- Added a separate vignette describing the `appStructure`
- Added vignette for deployment of shiny apps directly from
  `appStructure`
- Updated vignette for creating a new module using
  [`makeModule()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/makeModule.md)
- Updated ReadMe to refer to the public repository

## DarwinShinyModules 0.6.0

- Refreshed data from the DARWIN EU portal for database acronyms and
  descriptions
- added wrapper functions for creating (`moduleX()`,
  i.e. [`moduleIncidence()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/moduleIncidence.md))
  and launching (`shinyX()`,
  i.e. [`shinyIncidence()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/shinyIncidence.md))
  modules, which follow the existing `tableX()`, `plotX()` syntax for
  most of the analytical packages.
- TreatmentPatterns
  - Completely overhauled the module
  - Made the UI compatible with bootstrap 5 (bslib)
- CohortSurvival
  - Added facet and colour options
  - Made the UI compatible with bootstrap 5 (bslib)
- IncidencePrevalence
  - Added attrition tab for denominator cohorts
  - Added population tab for denominator cohorts

## DarwinShinyModules 0.5.0

- Re-factored the UI and back-end of `Incidence` and `Prevalence`
  modules
- Merged `Incidence` and `Prevalence` modules into `IncidencePrevalence`
- Added download arguments and button to `PlotStatic` by default
- Minor update to the `InputPanel` module back-end
- Added `DrugUtilisation`, `DrugRestart`, `Indication`, and `Treatment`
  modules to support `DrugUtilisation`
- Minor internal tweaks to `CohortSurvival`

## DarwinShinyModules 0.4.0

- Updated Cohort Survival
- Added
  [`darwinHeader()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/darwinHeader.md)
  and
  [`darwinFooter()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/darwinFooter.md)
  functions
- Load DARWIN EU logo from the package locally
- Added defaults for `Incidence` and `Prevalence` modules
- Updated footer for `bslib` and `shinydashboard`
- Added ability to customize plot rendering, by passing arguments
  through `...` to
  [`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html)
- Added `Flextable` module to support `flextable`
- Updated UI for modules to be more compatible when using `bslib`
- Added `Characteristics`, `CohortAttrition`,
  `LargeScaleCharacterisitcs` to support results from
  `CohortCharacteristics`
- Added `ReactableTable` module to support `reactable`

## DarwinShinyModules 0.3.2

- Exports the darwinHeader() and darwinFooter() functions to be used in
  bespoke shiny apps.
- Fixed multisession bug where inputs from one session affected all
  other sessions
- Updated CohortSurvival
  - Added Risk table using CohortSurvival::riskTable() in new tab for
    CohortSurvival
- Added docx download button for GTTable

## DarwinShinyModules 0.3.1

- Hotfix TreatmentPatterns modules

## DarwinShinyModules 0.3.0

### General

- \#88 Updated showcase app @mvankessel-EMC
- \#111 Updated `GTTable` module for its data to be updateable @ginberg
  @mvankessel-EMC
- \#120 Fixed unit tests @ginberg

### IncidencePrevalence

- \#98 Added denominator windows for the Incidence and Prevalence
  modules @ginberg
- \#105 Added option to remove the Confidence Intervals for the
  Incidence and Prevalence @ginberg
- \#108 Added `tableIncidence()` and `tablePrevalence()` results as an
  extra tab in their corresponding modules @ginberg
- \#106 Fixed issue with where intervals were not found when only one
  (i.e. overall) was present. @ginberg

### TreatmentPatterns

- \#50 Updated the existing TreatmentPatterns module, and renamed it to
  `TreatmentPathways`. @mvankessel-EMC
- \#50 Added the `EventDuration` module. @mvankessel-EMC

## DarwinShinyModules 0.2.2

- Updated showcase app
- Added Incidence Module
- Added Prevalence Module
- Deprecated IncidencePrevalence Module
- Fixed printing of updated labels when launching a shiny app from an
  appStructure
- Changed default title from `Plot` and `Table` to `NULL`

## DarwinShinyModules 0.2.1

- Allow more liberate labeling of menu.

## DarwinShinyModules 0.2.0

- Added Async
- Added copyright
- DatabaseOverview module
- Added StudyBackground module
- Database Modules supporting DatabaseConnector and DBI
- Added loading data vignette
- CohortSurvival Module

## DarwinShinyModules 0.1.1

- Added NEWS.md file.
- Fixed bug with unloading plots when session refreshes.
- Added `DrugExposureDiagnostics` module.
- Added builder function using `bslib`.
- Added last updated date in footer.
- Added copyright.
- Added
  [`makeModule()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/makeModule.md)
  function.
- Fixed vignette outputs.

## DarwinShinyModules 0.1.0

- Initial release
