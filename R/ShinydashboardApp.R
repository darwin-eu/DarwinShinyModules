# Copyright 2024 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

formatLabel <- function(label) {
  stringr::str_replace_all(string = label, pattern = "[\\!\\@\\#\\$\\%\\^\\*\\:\\;\\'\\\"]", replacement = "") |>
    stringr::str_replace_all(pattern = "[_]?\\s+", replacement = " ") |>
    stringr::str_replace_all(pattern = "[\\s\\(\\&\\)\\[\\]\\.\\,]", replacement = "_")
}

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
        sidebar = shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(private$modulesSidebar(private$.appStructure))),
        body = shinydashboard::dashboardBody(private$modulesBody(private$.appStructure))
      )
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
                tabName = formatLabel(label),
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
                    tabName = formatLabel(subLabel),
                    body
                  ))
                )
              } else {
                items <- append(
                  items,
                  list(shinydashboard::tabItem(
                    tabName = formatLabel(subLabel),
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
            shinydashboard::menuItem(
              text = label,
              tabName = formatLabel(label)
            )
          })
        } else {
          NULL
        }

        if (length(subItems) > 0) {
          shinydashboard::menuItem(
            text = menuLabel,
            tabName = formatLabel(menuLabel),
            shiny::tagList(subItems)
          )
        } else {
          shinydashboard::menuItem(
            text = menuLabel,
            tabName = formatLabel(menuLabel)
          )
        }
      })
    }
  )
)
