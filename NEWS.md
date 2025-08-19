# DarwinShinyModules 0.3.0
## General
* #88 Updated showcase app @mvankessel-EMC 
* #111 Updated `GTTable` module for its data to be updateable @ginberg @mvankessel-EMC
* #120 Fixed unit tests @ginberg

## IncidencePrevalence
* #98 Added denominator windows for the Incidence and Prevalence modules @ginberg 
* #105 Added option to remove the Confidence Intervals for the Incidence and Prevalence @ginberg 
* #108 Added `tableIncidence()` and `tablePrevalence()` results as an extra tab in their corresponding modules @ginberg 
* #106 Fixed issue with where intervals were not found when only one (i.e. overall) was present. @ginberg

## TreatmentPatterns
* #50 Updated the existing TreatmentPatterns module, and renamed it to `TreatmentPathways`. @mvankessel-EMC 
* #50 Added the `EventDuration` module. @mvankessel-EMC 

# DarwinShinyModules 0.2.2

* Updated showcase app
* Added Incidence Module
* Added Prevalence Module
* Deprecated IncidencePrevalence Module
* Fixed printing of updated labels when launching a shiny app from an appStructure
* Changed default title from `Plot` and `Table` to `NULL`

# DarwinShinyModules 0.2.1

* Allow more liberate labeling of menu.

# DarwinShinyModules 0.2.0

* Added Async
* Added copyright
* DatabaseOverview module
* Added StudyBackground module
* Database Modules supporting DatabaseConnector and DBI
* Added loading data vignette
* CohortSurvival Module

# DarwinShinyModules 0.1.1

* Added NEWS.md file.
* Fixed bug with unloading plots when session refreshes.
* Added `DrugExposureDiagnostics` module.
* Added builder function using `bslib`.
* Added last updated date in footer.
* Added copyright.
* Added `makeModule()` function.
* Fixed vignette outputs.

# DarwinShinyModules 0.1.0

* Initial release
