# modulesBody <- function(tabList) {
#   tabItems <- lapply(seq_len(length(tabList)), function(i) {
#     moduleList <- tabList[[i]]
#     headTabName <- names(tabList[i])
#     nestedTabName <- names(moduleList)
#     if (!is.null(nestedTabName)) {
#       lapply(seq_len(length(moduleList)), function(j) {
#         shinydashboard::tabItem(
#           tabName = nestedTabName[[j]],
#           sapply(moduleList[[j]], function(module) {
#             module$UI()
#           })
#         )
#       })
#     } else {
#       shinydashboard::tabItem(
#         tabName = headTabName,
#         sapply(moduleList, function(module) {
#           module$UI()
#         })
#       )
#     }
#   })
#   shinydashboard::dashboardBody(
#     do.call(shinydashboard::tabItems, args = tabItems)
#   )
# }

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

  shinydashboard::dashboardBody(
    do.call(shinydashboard::tabItems, args = l)
  )
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
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItems
    )
  )
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
