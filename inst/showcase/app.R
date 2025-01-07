library(DarwinShinyModules)
# Text module ----
code <- DarwinShinyModules::Text$new(
  markdown = c(
    "```r",
    readLines("./app.R"),
    "```"
  )
)

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

treatmentPatterns <- DarwinShinyModules::TreatmentPatterns$new(treatmentPathways = tp)

# IncidencePrevalence Module ----
inc <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/0.9.0/incidence.rds"))
pointPrev <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/0.9.0/pointPrevalence.rds"))
periodPrev <- readRDS(system.file(package = "DarwinShinyModules", "dummyData/IncidencePrevalence/0.9.0/periodPrevalence.rds"))

incMod <- IncidencePrevalence$new(data = inc)
pointPrevMod <- IncidencePrevalence$new(data = pointPrev)
periodPrevMod <- IncidencePrevalence$new(data = periodPrev)

# UI ----
ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(
    title = "Darwin Shiny Modules Showcase"
  ),

  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(text = "Code", tabName = "code"),
      shinydashboard::menuItem(text = "Tables", tabName = "tables"),
      shinydashboard::menuItem(text = "Plots", tabName = "plots"),
      shinydashboard::menuItem(text = "InputPanel", tabName = "inputPanel"),
      shinydashboard::menuItem(text = "TreatmentPatterns", tabName = "treatmentPatterns"),
      shinydashboard::menuItem(text = "IncidencePrevalence", tabName = "incidencePrevalence")
    )
  ),

  body = shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "code",
        code$UI()
      ),
      shinydashboard::tabItem(
        tabName = "tables",
        tableIris$UI(),
        tableMtcars$UI(),
        gtTableAirquality$UI()
      ),
      shinydashboard::tabItem(
        tabName = "plots",
        plotIris$UI(),
        plotIrisPlotly$UI(),
        plotNetwork$UI()
      ),
      shinydashboard::tabItem(
        tabName = "inputPanel",
        inputPanel$UI()
      ),
      shinydashboard::tabItem(
        tabName = "treatmentPatterns",
        treatmentPatterns$UI()
      ),
      shinydashboard::tabItem(
        tabName = "incidencePrevalence",
        incMod$UI(),
        pointPrevMod$UI(),
        periodPrevMod$UI()
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  code$server(input, output, session)
  tableIris$server(input, output, session)
  tableMtcars$server(input, output, session)
  gtTableAirquality$server(input, output, session)
  plotIris$server(input, output, session)
  plotIrisPlotly$server(input, output, session)
  plotNetwork$server(input, output, session)
  inputPanel$server(input, output, session)
  treatmentPatterns$server(input, output, session)
  incMod$server(input, output, session)
  pointPrevMod$server(input, output, session)
  periodPrevMod$server(input, output, session)
}

# Run ShinyApp ----
shiny::shinyApp(ui = ui, server = server)
