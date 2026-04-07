# darwinHeader

Creates the DARWIN EU (c) header banner.

## Usage

``` r
darwinHeader()
```

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
