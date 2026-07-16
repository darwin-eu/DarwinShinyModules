# launchFromDisk

Launches the shiny app from appStructure object from either a qs-file,
or from the object directly, using the provided `launchFun`. When the
qs-file does not exist, but the appStructure does, the appStructure is
saved to disk, to the provided `fileName`.

## Usage

``` r
launchFromDisk(
  filePath,
  launchFun = DarwinShinyModules::launchDarwinBslibApp,
  daemons = 4
)
```

## Arguments

- filePath:

  (`character`: `"./appStructure.qs"`) Path to the qs-file, containing
  the `appStructure`, or where to write the `appStructure` to.

- launchFun:

  (`launchFun`:
  [`DarwinShinyModules::launchDarwinBslibApp`](https://darwin-eu-dev.github.io/DarwinShinyModules/reference/launchDarwinBslibApp.md))
  launch function to use, may be any function that launches a shiny app
  from an appStructure object.

- daemons:

  (`numeric`: 2) Number of sub R proccess to kick off to load in results
  asynchronously.

## Value

`shiny.appobj` when the app is found, otherwise returns `NULL` invisible
