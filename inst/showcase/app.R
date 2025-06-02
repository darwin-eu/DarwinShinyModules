library(DarwinShinyModules)
library(TreatmentPatterns)
library(IncidencePrevalence)
library(visOmopResults)
library(shinydashboard)
library(bslib)
library(shinycssloaders)

# Text module ----
code <- DarwinShinyModules::Text$new(
  markdown = c(
    "```r",
    readLines("./app.R"),
    "```"
  )
)

abstract <- DarwinShinyModules::StudyBackground$new(
  background = "
  ## Introduction
  Some nicly formatted markdown.

  ## Methods
  Either as a character vector, or from a markdown-file, i.e. `abstract.md`
  ",
  EUPAS = "EUR0000000000"
)

dataPartners <- DarwinShinyModules::DatabaseOverview$new("IPCI", "CPRD GOLD", "SIDIAP")

# Table Modules ----
tableIris <- DarwinShinyModules::Table$new(data = iris, title = "Iris Data")
tableMtcars <- DarwinShinyModules::Table$new(data = mtcars, title = NULL, filter = "none")
gtTableAirquality <- DarwinShinyModules::GTTable$new(fun = gt::gt, args = list(data = airquality))

# Plot Modules ----
plotFun <- function(data) {
  ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = Sepal.Width, y = Sepal.Length)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(. ~ Species)
}

plotIris <- DarwinShinyModules::PlotStatic$new(fun = plotFun, args = list(data = iris))

plotIrisPlotly <- DarwinShinyModules::PlotPlotly$new(fun = plotFun, args = list(data = iris))

src <- c(
  "A", "A", "A", "A",
  "B", "B", "C", "C", "D"
)
target <- c(
  "B", "C", "D", "J",
  "E", "F", "G", "H", "I"
)

plotNetwork <- DarwinShinyModules::PlotWidget$new(fun = networkD3::simpleNetwork, args = list(Data = data.frame(src, target)))


# InputPanel Module ----
inputPanel <- DarwinShinyModules::InputPanel$new(
  funs = list(
    text = shiny::textInput,
    select = shiny::selectInput
  ),
  args = list(
    text = list(
      inputId = "text",
      label = "Text"
    ),
    select = list(
      inputId = "select",
      label = "Select",
      choices = c("A", "B", "C")
    )
  )
)

# TreatmentPatterns Module ----
tp <- read.csv(system.file(
  package = "DarwinShinyModules",
  "dummyData/TreatmentPatterns/3.0.0", "treatment_pathways.csv"
))

tpMod <- DarwinShinyModules::TreatmentPatterns$new(treatmentPathways = tp)

# IncidencePrevalence Module ----
inc <- omopgenerics::importSummarisedResult(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/1.2.0/incidence.csv"))
prev <- omopgenerics::importSummarisedResult(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"))

incMod <- Incidence$new(data = inc)
prevMod <- Prevalence$new(data = prev)

appStructure <- list(
  Code = code,
  Abstract = abstract,
  Databases = dataPartners,
  Tables = list(
    DT = list(tableIris, tableMtcars),
    gt = gtTableAirquality
  ),
  Plost = list(
    Static = plotIris,
    Plotly = plotIrisPlotly,
    Widget = plotNetwork
  ),
  InputPanel = inputPanel,
  `TreatmentPatterns` = tpMod,
  `IncidencePrevalence` = list(
    Incidence = incMod,
    Prevalence = prevMod
  )
)

launchDarwinDashboardApp(appStructure)
