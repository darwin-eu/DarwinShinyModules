# darwinFooter

Creates the DARWIN EU (c) footer.

## Usage

``` r
darwinFooter(type = "shinydashboard")
```

## Arguments

- type:

  for which app type the footer should be generated

## Value

`tagList`

## Examples

``` r
ui <- shiny::fluidPage(
  darwinHeader(),
  shiny::p("Content"),
  darwinFooter()
)

server <- function(input, output, session) {}

if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
