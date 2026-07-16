# loadAppStructure

Loads an `appStructure` object from a downloaded tar.gz file.

## Usage

``` r
loadAppStructure(
  filePath,
  appStructureFileName = "appStructure.qs",
  subDir = NULL
)
```

## Arguments

- filePath:

  (`character(1)`) Path to the tar.gz file downlaoded from posit
  connect.

- appStructureFileName:

  (`character(1)`) Name of the appStructure file.

- subDir:

  (`character(1)`: `NULL`) Optional deviation in the sub directory, if
  the tar.gz is not structured identically to how posit connect
  structures these files.

## Value

`list` appStructure
