DarwinShinyModules
=================

[![Build Status](https://github.com/darwin-eu-dev/DarwinShinyModules/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu-dev/DarwinShinyModules/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/DarwinShinyModules/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu-dev/DarwinShinyModules?branch=main)

DarwinShinyModules is an R package containing shiny modules that can be used within shiny result interfaces.

The Darwin tools often provide shiny interfaces for viewing and exploring results.  Many of these shiny apps have overlapping features. To ensure consistency we have created a repository containing useful shiny modules that can be used in multiple result explorers.

Current Modules
========


Technology
==========
  DarwinShinyModules is an R package that uses the R shiny library.

System Requirements
===================
  Requires R (version 3.3.0 or higher).

Installation
============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. To install the latest stable version:

  ```
install.packages(remotes)
remotes::install_github('darwin-eu-dev/DarwinShinyModules')
```

User Documentation
==================
  Documentation can be found on the [package website](https://ohdsi.github.io/DarwinShinyModules/).

PDF versions of the documentation are also available:
Vignette: [AddingShinyModules.pdf](https://github.com/darwin-eu-dev/DarwinShinyModules/blob/main/inst/doc/AddingShinyModules.pdf)


Contributing
============
* Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.
* This [website](https://mastering-shiny.org/scaling-modules.html) may be helpful if you want to see an introduction into how to write shiny modules

License
=======
  DarwinShinyModules is licensed under Apache License 2.0

Development
===========
  DarwinShinyModules is being developed in R Studio.


# Acknowledgements

- The package is maintained by <add> and has been developed with major contributions from <add>
