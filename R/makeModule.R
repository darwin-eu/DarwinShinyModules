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
    #' @description
    #' Initializer method
    #'
    #' @param ui UI element passed to the `makeModule()` function.
    #' @param server (`function`) Server function passed to the `makeModule()` function
    #'
    #' @return `self`
    initialize = function(server, ui) {
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
      if ("session" %in% serverFunArgs) {
        do.call(
          what = private$.varServer,
          args = list(input = input, output = output, session = session)
        )
      } else {
        do.call(
          what = private$.varServer,
          args = list(input = input, output = output)
        )
      }
    }
  )
)


#' makeModule
#'
#' Function to make a `ShinyModule` from an UI element and server function.
#'
#' @details
#' The function allows for easy migration between bespoke shiny code and the
#' modular framework in `DarwinShinyModules`, without having to implement an
#' `R6` class. One caveat is, is the generated module is completely isolated.
#' Meaning that the module does not allow other modules to read from or write
#' to any defined (reactive) variables in the provided server function.
#'
#' @param ui Shiny UI elements i.e. a `shiny.tag.list`, or similar ui objects
#' from packages like `shiny`, `shinydashboard`, or `bslib`
#' @param server (`function`) A server function with atleast a `input` and
#' `output` argument.
#' @param namespace (`character`: `NULL`) Namespace used in the ui element.
#'
#' @returns `ShinyModule`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(shiny)
#'
#' ui <- tagList(p("My UI"))
#' server <- function(input, output, session) {
#'   # Do stuff
#' }
#'
#' mod <- makeModule(ui, server)
#'
#' if (interactive()) {
#'   preview(mod)
#' }
makeModule <- function(ui, server, namespace = NULL) {
  mod <- GenericModule$new(ui = ui, server = server)
  mod$moduleId <- namespace
  return(mod)
}
