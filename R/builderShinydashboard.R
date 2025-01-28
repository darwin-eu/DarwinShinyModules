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
  items <- list()

  for (i in seq_len(length(tabList))) {
    label <- names(tabList)[i]
    if ("list" %in% class(tabList[[i]])) {
      subLabels <- names(tabList[[i]])
      if (is.null(subLabels)) {
        body <- lapply(tabList[[i]], function(module) {
          module$UI()
        }) |>
          shiny::tagList()
        items <- append(
          items,
          list(shinydashboard::tabItem(
            tabName = label,
            body
          ))
        )
      } else {
        for (subLabel in subLabels) {
          if ("list" %in% class(tabList[[i]][[subLabel]])) {
            body <- lapply(tabList[[i]][[subLabel]], function(module) {
              module$UI()
            }) |>
              shiny::tagList()
            items <- append(
              items,
              list(shinydashboard::tabItem(
                tabName = subLabel,
                body
              ))
            )
          } else {
            items <- append(
              items,
              list(shinydashboard::tabItem(
                tabName = subLabel,
                tabList[[i]][[subLabel]]$UI()
              ))
            )
          }
        }
      }
    } else {
      items <- append(
        items,
        list(shinydashboard::tabItem(
          tabName = label,
          tabList[[i]]$UI()
        ))
      )
    }
  }
  do.call(shinydashboard::tabItems, items)
}

modulesSidebar <- function(tabList) {
  lapply(seq_len(length(tabList)), function(i) {
    menuLabel <- names(tabList[i])

    subItems <- if ("list" %in% class(tabList[[i]])) {
      labels <- names(tabList[[i]])
      lapply(labels, function(label) {
        shinydashboard::menuItem(text = label, tabName = label)
      })
    } else {
      NULL
    }

    if (length(subItems) > 0) {
      shinydashboard::menuItem(
        text = stringr::str_replace_all(menuLabel, pattern = "_", replacement = " "),
        tabName = menuLabel,
        shiny::tagList(subItems)
      )
    } else {
      shinydashboard::menuItem(
        text = stringr::str_replace_all(menuLabel, pattern = "_", replacement = " "),
        tabName = menuLabel
      )
    }
  })
}

assertAppStructure <- function(appStructure) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertList(appStructure, types = c("ShinyModule", "list"), add = assertions)
  checkmate::assertNamed(appStructure, type = "named", add = assertions)
  checkmate::reportAssertions(assertions)
}

#' dashboardApp
#'
#' @param appStructure (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' base <- Text$new("**base**")
#' nested_a <- Text$new("**nested A**")
#' nested_b <- Text$new("**nested B**")
#' sub_a <- Text$new("**sub A**")
#' sub_b <- Text$new("**sub B**")
#' comb_a <- Text$new("**comb A**")
#' comb_b <- Text$new("**comb B**")
#' comb_c <- Text$new("**comb C**")
#'
#' if (interactive()) {
#'   appStructure <- list(
#'     base = base,
#'     nested = list(nested_a, nested_b),
#'     nested_sub = list(
#'       sub_a = sub_a,
#'       sub_b = sub_b
#'     ),
#'     nested_combined = list(
#'       comb_a_b = list(comb_a, comb_b),
#'       comb_c = comb_c
#'     )
#'   )
#'
#'   dashboardApp(appStructure)
#' }
dashboardApp <- function(appStructure, title = NULL) {
  assertAppStructure(appStructure)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE)
  ui <- shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(title = title),
    sidebar = shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(modulesSidebar(appStructure))),
    body = shinydashboard::dashboardBody(modulesBody(appStructure))
  )

  server <- function(input, output, session) {
    modules <- unlist(appStructure)
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}
