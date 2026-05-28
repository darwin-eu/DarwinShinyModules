# DatabaseOverview Module Class

The DatabaseOverview module displays information about data partners,
and links to the Portal and data partner website.

## Super class

[`ShinyModule`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.md)
-\> `DatabaseOverview`

## Active bindings

- `table`:

  (`Table`) Table module

## Methods

### Public methods

- [`DatabaseOverview$new()`](#method-DatabaseOverview-initialize)

- [`DatabaseOverview$clone()`](#method-DatabaseOverview-clone)

Inherited methods

- [`ShinyModule$UI()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-UI)
- [`ShinyModule$getReactiveValues()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-getReactiveValues)
- [`ShinyModule$server()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-server)
- [`ShinyModule$validate()`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/ShinyModule.html#method-validate)

------------------------------------------------------------------------

### `DatabaseOverview$new()`

Initializer method

#### Usage

    DatabaseOverview$new(...)

#### Arguments

- `...`:

  (`character(n)`) Individual database names. See Details for the
  supported data partners.

#### Details

- A:

  "APHM"

- B:

  "BIFAP"

- C:

  "CDW Bordeaux"  
  "CPRD GOLD" "CRN"

- D:

  "DK-DHR"

- E:

  "EBB"  
  "EMDB - ULSEDV"  
  "EMDB - ULSGE"  
  "EMDB - ULSRA"

- F:

  "FinOMOP - ACI Varha"  
  "FinOMOP - HILMO"  
  "FinOMOP - HUS"  
  "FinOMOP - TaUH Pirha"

- H:

  "H12O"  
  "HARMONY Platform"  
  "HARMONY - ALL"  
  "HARMONY - AML"  
  "HARMONY - CML"  
  "HARMONY - MM"

- I:

  "IMASIS"  
  "InGef RDB"  
  "IPCI"  
  "IQVIA DA Germany"  
  "IQVIA LPD Belgium"

- N:

  "NAJS"  
  "NCR"  
  "NLHR"  
  "NLHR@UiO:PERINATAL"  
  "NNRD"

- P:

  "PGH"  
  "PRISIB"  
  "POLIMI"

- S:

  "SIDIAP"  
  "SNDS"  
  "SUCD"

- U:

  "UKBB"  
  "ULSM-RT"

- V:

  "VID"

------------------------------------------------------------------------

### `DatabaseOverview$clone()`

The objects of this class are cloneable with this method.

#### Usage

    DatabaseOverview$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(DarwinShinyModules)

databaseOverview <- DatabaseOverview$new("IPCI", "CPRD GOLD", "SIDIAP")

if (interactive()) {
  preview(databaseOverview)
}
```
