modulesBody <- function(tabList) {
  tabItems <- lapply(seq_len(length(tabList)), function(i) {
    moduleList <- tabList[[i]]
    label <- names(tabList)[i]
    shinydashboard::tabItem(
      tabName = label,
      sapply(moduleList, function(module) {
        module$UI()
      })
    )
  })
  shinydashboard::dashboardBody(
    do.call(shinydashboard::tabItems, tabItems)
  )
}

modulesSideBar <- function(tabList) {
  menuItems <- lapply(seq_len(length(tabList)), function(i) {
    moduleList <- tabList[[i]]
    label <- names(tabList)[i]
    lapply(moduleList, function(module) {
      shinydashboard::menuItem(
        text = stringr::str_replace_all(label, pattern = "_", replacement = " "),
        tabName = label
      )
    })
  }) |>
    unlist(recursive = FALSE)
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItems
    )
  )
}

checkReference <- function(moduleList) {
  refNames <- sapply(moduleList, function(module) {
    if (module$reference == "") {
      module$reference <- module$moduleName
    }
  })
}

dashboardApp <- function(moduleList, title = NULL) {
  ui <- shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(title = NULL),
    sidebar = modulesSideBar(moduleList),
    body = modulesBody(moduleList)
  )

  server = function(input, output, session) {
    modules <- unlist(moduleList)
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}

# table1 <- Table$new(iris)
# table2 <- Table$new(mtcars)
#
# moduleList <- list(
#   Iris = list(table1),
#   MT_Cars = list(table2)
# )
#
# dashboardApp(moduleList)
