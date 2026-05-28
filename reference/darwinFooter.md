# darwinFooter

Creates the DARWIN EU (c) footer.

## Usage

``` r
darwinFooter(type = "shinydashboard", deployDate = Sys.Date())
```

## Arguments

- type:

  for which app type the footer should be generated

- deployDate:

  Date when the app is deployed

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
