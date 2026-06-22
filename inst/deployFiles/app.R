library(DarwinShinyModules)

# These would be loaded in when DarwinShinyModules is loaded.
library(mirai)
library(promises)
library(future)
library(R6)

# launch the shiny app
launchFromDisk("appStructure.qs")
