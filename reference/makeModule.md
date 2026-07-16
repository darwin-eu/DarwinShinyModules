# makeModule

`makeModule` allows for easy migration between bespoke shiny code and
the modular framework in `DarwinShinyModules`, without having to
implement an `R6` class.

## Usage

``` r
makeModule(ui, server, namespace = NULL, data = NULL)
```

## Arguments

- ui:

  Shiny UI elements i.e. a `shiny.tag.list`, or similar ui objects from
  packages like `shiny`, `shinydashboard`, or `bslib`

- server:

  (`function`) A server function with at least a `input` and `output`
  argument.

- namespace:

  (`character`: `NULL`) Namespace used in the ui element.

- data:

  (`NULL`) Data to use in the `server` function. Will be available in
  the `server` function definition as `data`. If multiple data objects
  are needed, you can pack them up in a
  [`list()`](https://rdrr.io/r/base/list.html) and unpack them in your
  sever definition, see the examples. `data` is still over-writable
  after the fact, with `myMod$data <- updatedData`

## Value

`ShinyModule`

## Details

The `data` argument will be available in the defined `server` function
at run time, regardless if you pass it to the `server` function or not.
`data` is stored on the returned `ShinyModule` object which makes it
entirely self sufficient.

## Examples

``` r
library(DarwinShinyModules)
library(shiny)
#> 
#> Attaching package: ‘shiny’
#> The following objects are masked from ‘package:DT’:
#> 
#>     dataTableOutput, renderDataTable

ui <- fluidPage(
  tableOutput(NS("myMod", "table"))
)

# Notice that we do NOT pass a `data` argument, but it is still available in
# the `server` function
server <- function(input, output, session) {
  output$table <- renderTable({
    data
  })
}

mod <- makeModule(ui, server, namespace = "myMod", data = iris)

if (interactive()) {
  preview(mod)
}

# Multiple data structures:
ui <- fluidPage(
  tableOutput(NS("myMod", "tableIris")),
  tableOutput(NS("myMod", "tableCars"))
)

server <- function(input, output, session) {
  datIris <- data$iris
  datCars <- data$mtcars

  output$tableIris <- renderTable({
    datIris
  })

  output$tableCars <- renderTable({
    datCars
  })
}

mod <- makeModule(ui, server, namespace = "myMod", data = list(iris = iris, mtcars = mtcars))

if (interactive()) {
  preview(mod)
}
```
