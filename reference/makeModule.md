# makeModule

Function to make a `ShinyModule` from an UI element and server function.

## Usage

``` r
makeModule(ui, server, namespace = NULL)
```

## Arguments

- ui:

  Shiny UI elements i.e. a `shiny.tag.list`, or similar ui objects from
  packages like `shiny`, `shinydashboard`, or `bslib`

- server:

  (`function`) A server function with atleast a `input` and `output`
  argument.

- namespace:

  (`character`: `NULL`) Namespace used in the ui element.

## Value

`ShinyModule`

## Details

The function allows for easy migration between bespoke shiny code and
the modular framework in `DarwinShinyModules`, without having to
implement an `R6` class. One caveat is, is the generated module is
completely isolated. Meaning that the module does not allow other
modules to read from or write to any defined (reactive) variables in the
provided server function.

## Examples

``` r
library(DarwinShinyModules)
library(shiny)
#> 
#> Attaching package: ‘shiny’
#> The following objects are masked from ‘package:DT’:
#> 
#>     dataTableOutput, renderDataTable

ui <- tagList(p("My UI"))
server <- function(input, output, session) {
  # Do stuff
}

mod <- makeModule(ui, server)

if (interactive()) {
  preview(mod)
}
```
