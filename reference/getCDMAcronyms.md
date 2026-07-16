# getCDMAcronyms

Returns a character vector containing all the data source acronyms from
the DARWIN EU Portal

## Usage

``` r
getCDMAcronyms()
```

## Value

`character(n)`

## Examples

``` r
getCDMAcronyms()
#> # A tibble: 49 × 2
#>    acronym           acronym_multi_line   
#>    <chr>             <chr>                
#>  1 SUCD              "SUCD"               
#>  2 CRN               "CRN"                
#>  3 IQVIA DA Germany  "IQVIA\nDA Germany"  
#>  4 H12O              "H12O"               
#>  5 InGef RDB         "InGef RDB"          
#>  6 EBB               "EBB"                
#>  7 IMASIS            "IMASIS"             
#>  8 FinOMOP-ACI Varha "FinOMOP-\nACI Varha"
#>  9 IQVIA US - PMTX+  "IQVIA\nUS - PMTX+"  
#> 10 HARMONY-CML       "HARMONY-\nCML"      
#> # ℹ 39 more rows
```
