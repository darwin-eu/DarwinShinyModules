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

#' Preview
#'
#' Launches a shiny app with the modules' `server()` and `UI()` methods.
#'
#' @param modules ([ShinyModule]) A vector of module objects.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' table <- Table$new(data = iris)
#'
#' if (interactive()) {
#'   preview(table)
#' }
preview <- function(modules) {
  assertions <- checkmate::makeAssertCollection()
  modules <- if (all(class(modules) != "list")) {
    list(modules)
  } else {
    modules
  }

  checkmate::assertList(
    .var.name = "modules",
    x = modules,
    types = "ShinyModule",
    min.len = 1,
    add = assertions
  )

  checkmate::reportAssertions(assertions)

  ui <- shiny::fluidPage(
    shiny::tagList(
      lapply(modules, function(module) {
        module$UI()
      })
    )
  )

  server <- function(input, output, session) {
    for (module in modules) {
      module$server(input, output, session)
    }
  }

  shiny::shinyApp(ui, server)
}
