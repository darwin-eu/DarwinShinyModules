# deployAppStructure

Wrapper for
[`rsconnect::deployApp`](https://rstudio.github.io/rsconnect/reference/deployApp.html).
Deploys a shiny app from the appStructure

## Usage

``` r
deployAppStructure(
  appStructure,
  appDir = tempfile(),
  launchFun = "launchDarwinBslibApp",
  daemons = 4,
  pkgs = NULL,
  ...
)
```

## Arguments

- appStructure:

  (`list`: `NULL`) appStructure object, i.e. a named list of ShinyModule
  objects or (named) lists containing (named) ShinyModule objects.
  Representing the navigation (side) bar of the shiny app.

- appDir:

  (`character(1)`: [`tempfile()`](https://rdrr.io/r/base/tempfile.html))
  File path to the directory where app files will be written to, and the
  app is being deployed from. See
  [`rsconnect::deployApp`](https://rstudio.github.io/rsconnect/reference/deployApp.html)
  for further details.

- launchFun:

  (`character(1)`: `"launchDarwinBslibApp"`) Launch function as
  character

- daemons:

  (`numeric(1)`: `4`) Number of daemons (sub-processes) to launch with
  the shiny app.

- pkgs:

  (`character(n)`: `NULL`) Additional packages that should be installed
  and loaded when deploying.

- ...:

  Arguments for
  [`rsconnect::deployApp`](https://rstudio.github.io/rsconnect/reference/deployApp.html)

## Value

`NULL`
