statusCompleted <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("check"),
    badgeStatus = NULL,
    headerText = "Study Completed"
  )
}

statusOngoing <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("rotate-right"),
    badgeStatus = NULL,
    headerText = "Study Ongoing"
  )
}

statusStopped <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("exclamation"),
    badgeStatus = NULL,
    headerText = "Study Stopped"
  )
}

StudyStatus <- function(type) {
  switch(type,
         "stop" = statusStopped(),
         "complete" = statusCompleted(),
         "ongoing" = statusOngoing())
}

modulesBody <- function(tabList) {
  l <- lapply(seq_len(length(tabList)), function(i) {
    nav <- tabList[[i]]
    lapply(seq_len(length(nav)), function(j) {
      subNav <- nav[[j]]
      if (all(class(subNav) == "list")) {
        lapply(subNav, function(module) {
          return(
            shinydashboard::tabItem(
              tabName = names(nav[j]),
              module$UI()
            )
          )
        }) |>
          unlist(recursive = FALSE)
      } else {
        module <- subNav
        return(
          shinydashboard::tabItem(
            tabName = names(tabList[i]),
            module$UI()
          )
        )
      }
    })
  }) |> unlist(recursive = FALSE)

  l <- lapply(l, function(item) {
    class(item) <- "shiny.tag"
    item
  })

  do.call(shinydashboard::tabItems, args = l)
}

modulesSideBar <- function(tabList) {
  menuItems <- lapply(seq_len(length(tabList)), function(i) {
    moduleList <- tabList[[i]]
    text <- names(tabList)[i]

    if (length(moduleList) > 1) {
      subTexts <- names(moduleList)
      shinydashboard::menuItem(
        text = text,
        lapply(subTexts, function(subText) {
          shinydashboard::menuSubItem(
            text = stringr::str_replace_all(subText, pattern = "_", replacement = " "),
            tabName = subText
          )
        })
      )
    } else {
      shinydashboard::menuItem(
        text = stringr::str_replace_all(text, pattern = "_", replacement = " "),
        tabName = text
      )
    }
  })
  menuItems
}

#' dashboardApp
#'
#' @param moduleList (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(ggplot2)
#'
#' mtcarsTable <- Table$new(data = mtcars)
#' irisTable <- Table$new(data = iris)
#'
#' irisPlotFun <- function(data) {
#'   ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'     geom_point() +
#'     theme_bw()
#' }
#'
#' irisPlot <- PlotStatic$new(fun = irisPlotFun, args = list(data = iris))
#'
#' if (interactive()) {
#'   dashboardApp(
#'     moduleList = list(
#'       # Menu item `Iris`
#'       Iris = list(irisPlot, irisTable),
#'       # Menu item `MT Cars`
#'       MT_Cars = list(mtcarsTable)
#'     )
#'   )
#' }
dashboardApp <- function(moduleList, title = NULL) {
  ui <- shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(title = title),
    sidebar = modulesSideBar(moduleList),
    body = shinydashboard::dashboardBody(modulesBody(moduleList))
  )

  server = function(input, output, session) {
    modules <- unlist(moduleList)
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}

darwinHeader <- function() {
  shiny::tagList(
    shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "style.css")),
    shiny::tags$body(
      shiny::tags$div(
        class = "g-block size-100 darwin-top-line",
        shiny::tags$div(
          id = "custom-2346-particle",
          class = "g-content g-particle"
        )
      ),
      shiny::tags$div(
        class = "g-grid",
        shiny::tags$div(
          class = "header-background-image",
          style = "padding-top&#x3A;&#x20;40px&#x20;&#x21;important&#x3B;&#x20;padding-left&#x3A;&#x20;25px&#x20;&#x21;important&#x3B;",
          shiny::tags$div(
            id = "logo-1935-particle",
            class = "g-content g-particle",
            shiny::tags$a(
              href = "/", class = "g-logo darwin-eu-logo", target = "_self", title = "", rel = "home",
              shiny::tags$img(
                src = "https://darwin-eu.org/templates/rt_horizon/custom/images/darwin-eu-logo.png",
                height = "100px",
                alt = ""
              )
            )
          )
        )
      )
    )
  )
}

darwinFooter <- function() {
  shiny::tags$footer(
    shiny::h6(
      sprintf(
        "Generated with DarwinShinyModules %s",
        utils::packageVersion("DarwinShinyModules"))
    )
  )
}

#' darwinApp
#'
#' @param moduleList (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#' @param studyStatus (`character(1)`: `"ongoing"`) Status of the study
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(ggplot2)
#'
#' mtcarsTable <- Table$new(data = mtcars)
#' irisTable <- Table$new(data = iris)
#'
#' irisPlotFun <- function(data) {
#'   ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'     geom_point() +
#'     theme_bw()
#' }
#'
#' irisPlot <- PlotStatic$new(fun = irisPlotFun, args = list(data = iris))
#'
#' if (interactive()) {
#'   darwinApp(
#'     moduleList = list(
#'       # Menu item `Iris`
#'       Iris = list(irisPlot, irisTable),
#'       # Menu item `MT Cars`
#'       MT_Cars = list(mtcarsTable)
#'     )
#'   )
#' }
darwinApp <- function(moduleList, title = "", studyStatus = "ongoing") {
  shiny::addResourcePath(
    prefix = "www/img",
    directoryPath = system.file("www/img", package = "DarwinShinyModules")
  )

  ui <- shiny::tags$body(
    shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "theme.css")),
    darwinHeader(),
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = title,
        titleWidth = 300,
        StudyStatus(studyStatus)
      ),
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          modulesSideBar(moduleList)
        ), width = 300
      ),
      body = shinydashboard::dashboardBody(modulesBody(moduleList))
    ),
    darwinFooter()
  )

  server = function(input, output, session) {
    modules <- unlist(moduleList)
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}
