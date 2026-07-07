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

#' @title Generic Module Class
#'
#' @description
#' This class is an internal class used to programmatically generate modules from
#' an UI element and server function, with the `makeModule()` function.
#'
#' @noMd
#' @noRd
#' @keywords internal
GenericModule <- R6::R6Class(
  classname = "GenericModule",
  inherit = ShinyModule,
  active = list(
    #' @field moduleId (`character`) Overwrite from `ShinyModule`
    moduleId = function(moduleId) {
      if (missing(moduleId)) {
        return(private$.moduleId)
      } else {
        checkmate::assertCharacter(moduleId, null.ok = TRUE, len = 1)
        private$.moduleId <- moduleId
      }
    },

    #' @field varUI UI element passed to the `makeModule()` function.
    varUI = function() {
      return(private$.varUI)
    },

    #' @field varServer (`function`) Server function passed to the `makeModule()` function
    varServer = function() {
      return(private$.varServer)
    }
  ),
  public = list(
    #' @field data Data object to use in the defined `server`.
    data = NULL,

    #' @description
    #' Initializer method
    #'
    #' @param ui UI element passed to the `makeModule()` function.
    #' @param server (`function`) Server function passed to the `makeModule()` function
    #' @param data Data to use
    #'
    #' @return `self`
    initialize = function(server, ui, data = NULL, ...) {
      super$initialize(...)
      self$data <- data
      private$.varUI <- ui
      private$.varServer <- server
      return(invisible(self))
    }
  ),
  private = list(
    .varUI = NULL,
    .varServer = NULL,
    .UI = function() {
      private$.varUI
    },
    .server = function(input, output, session = shiny::getDefaultReactiveDomain()) {
      serverFunArgs <- list(args(private$.varServer))

      formalArgs <- formals(private$.varServer)
      formalArgs$data <- bquote()
      formals(private$.varServer) <- formalArgs

      if ("session" %in% serverFunArgs) {
        do.call(
          what = private$.varServer,
          args = list(input = input, output = output, session = session, data = self$data)
        )
      } else {
        do.call(
          what = private$.varServer,
          args = list(input = input, output = output, data = self$data)
        )
      }
    }
  )
)


#' makeModule
#'
#' `makeModule` allows for easy migration between bespoke shiny code and the
#' modular framework in `DarwinShinyModules`, without having to implement an
#' `R6` class.
#'
#' @details
#' The `data` argument will be available in the defined `server` function at
#' run time, regardless if you pass it to the `server` function or not. `data`
#' is stored on the returned `ShinyModule` object which makes it entirely self
#' sufficient.
#'
#' @param ui Shiny UI elements i.e. a `shiny.tag.list`, or similar ui objects
#' from packages like `shiny`, `shinydashboard`, or `bslib`
#' @param server (`function`) A server function with at least a `input` and
#' `output` argument.
#' @param namespace (`character`: `NULL`) Namespace used in the ui element.
#' @param data (`NULL`) Data to use in the `server` function. Will be available
#' in the `server` function definition as `data`. If multiple data objects are
#' needed, you can pack them up in a `list()` and unpack them in your sever
#' definition, see the examples. `data` is still over-writable after the fact,
#' with `myMod$data <- updatedData`
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(shiny)
#'
#' ui <- fluidPage(
#'   tableOutput(NS("myMod", "table"))
#' )
#'
#' # Notice that we do NOT pass a `data` argument, but it is still available in
#' # the `server` function
#' server <- function(input, output, session) {
#'   output$table <- renderTable({
#'     data
#'   })
#' }
#'
#' mod <- makeModule(ui, server, namespace = "myMod", data = iris)
#'
#' if (interactive()) {
#'   preview(mod)
#' }
#'
#' # Multiple data structures:
#' ui <- fluidPage(
#'   tableOutput(NS("myMod", "tableIris")),
#'   tableOutput(NS("myMod", "tableCars"))
#' )
#'
#' server <- function(input, output, session) {
#'   datIris <- data$iris
#'   datCars <- data$mtcars
#'
#'   output$tableIris <- renderTable({
#'     datIris
#'   })
#'
#'   output$tableCars <- renderTable({
#'     datCars
#'   })
#' }
#'
#' mod <- makeModule(ui, server, namespace = "myMod", data = list(iris = iris, mtcars = mtcars))
#'
#' if (interactive()) {
#'   preview(mod)
#' }
makeModule <- function(ui, server, namespace = NULL, data = NULL) {
  mod <- GenericModule$new(ui = ui, server = server, data = data)
  mod$moduleId <- namespace
  return(mod)
}
