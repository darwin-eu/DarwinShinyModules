ShinydashboardApp <- R6::R6Class(
  classname = "ShinydashboardApp",
  inherit = App,

  ## Public ----
  public = list(
    ### Methods ----
    initialize = function(appStructure, title = "") {
      super$initialize(appStructure)
      private$.title <- title
    },

    UI = function() {
      shinydashboard::dashboardPage(
        header = shinydashboard::dashboardHeader(title = private$.title),
        sidebar = shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(modulesSidebar(private$.appStructure))),
        body = shinydashboard::dashboardBody(modulesBody(private$.appStructure))
      )
    },

    server = function(input, output, session) {
      modules <- unlist(private$.appStructure)
      for (module in modules) {
        module$server(input, output, session)
      }
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .title = "",

    ### Methods ----
    modulesBody = function(tabList) {
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
    },

    modulesSidebar = function(tabList) {
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
  )
)
