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

# Classes ----
DarwinDashboardApp <- R6::R6Class(
  classname = "DarwinDashboardApp",
  inherit = ShinydashboardApp,

  ## Public ----
  public = list(
    launch = function() {
      shiny::addResourcePath(
        prefix = "www/img",
        directoryPath = system.file("www/img", package = "DarwinShinyModules")
      )
      super$launch()
    },

    UI = function() {
      shiny::tags$body(
        shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "theme.css")),
        darwinHeader(),
        shinydashboard::dashboardPage(
          header = shinydashboard::dashboardHeader(
            title = private$.title,
            titleWidth = 300
          ),
          sidebar = shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              private$modulesSidebar(private$.appStructure)
            ), width = 300
          ),
          body = shinydashboard::dashboardBody(private$modulesBody(private$.appStructure))
        ),
        darwinFooter()
      )
    }
  )
)

# Internal Functions ----
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
    style = "padding: 0px; text-align: center; position: fixed; bottom: 0; width: 100%;",
    shiny::h6(
      sprintf(
        "Generated with DarwinShinyModules %s | Deployed on: %s | (c) %s - 2023 European Medicines Agency. All rights reserved. Certain parts are licensed under conditions to the European Medicines Agency.",
        utils::packageVersion("DarwinShinyModules"),
        Sys.Date(),
        substr(Sys.Date(), start = 1, stop = 4)
      )
    )
  )
}
