# Function that creates a stand-alone shiny app from a given app structure.

Function that creates a stand-alone shiny app from a given app
structure.

## Usage

``` r
createApp(path, appStructure, theme = "shinymodules", additionalFiles = c())
```

## Arguments

- path:

  path where shiny app will be stored

- appStructure:

  application structure as a list

- theme:

  theme as a character, it can be one of 'shinymodules' (default),
  'bslib', 'bslib-darwin' or 'shinymodules-darwin'

- additionalFiles:

  optional vector of files to be copied

## Examples

``` r
library(DarwinShinyModules)

base <- Text$new("**base**")
nested_a <- Text$new("**nested A**")
nested_b <- Text$new("**nested B**")
sub_a <- Text$new("**sub A**")
sub_b <- Text$new("**sub B**")
comb_a <- Text$new("**comb A**")
comb_b <- Text$new("**comb B**")
comb_c <- Text$new("**comb C**")

appStructure <- list(
   base = base,
   nested = list(nested_a, nested_b),
   nested_sub = list(
     sub_a = sub_a,
     sub_b = sub_b
   ),
   nested_combined = list(
     comb_a_b = list(comb_a, comb_b),
     comb_c = comb_c
   )
 )

if (interactive()) {
  createApp(path = tempdir(),
            appStructure = appStructure,
            theme = 'shinymodules-darwin')
}
```
