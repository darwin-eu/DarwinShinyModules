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

#' @title Bridge Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Bridge module that "birdges" multiple modules with bespoke server code.
#'
#' @details
#' The Bridge module links two or more modules together with user defined server code.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' table <- Table$new(data = mtcars)
#'
#' inputPanel <- InputPanel$new(
#'   funs = list(
#'   inputSpecies = shiny::selectInput
#'   ),
#'   args = list(
#'     inputSpecies = list(
#'       inputId = "inputSpecies",
#'       label = "Select Species",
#'       choices = unique(iris$Species),
#'       selected = unique(iris$Species)[1]
#'     )
#'   )
#' )
#'
#' bridgeFun <- function(input, output, session) {
#'   shiny::observeEvent(inputPanel$inputValues$inputSpecies, {
#'     table$data <- iris %>%
#'       dplyr::filter(.data$Species == inputPanel$inputValues$inputSpecies)
#'   })
#' }
#'
#' bridge <- Bridge$new(inputPanel, table, bridgeFun = bridgeFun)
#'
#' if (interactive()) {
#'   preview(bridge)
#' }
Bridge <- R6::R6Class(
  classname = "Bridge",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field modules (`list`) List of modules.
    modules = function() {
      return(private$.modules)
    },

    #' @field birdgeFun (`function`) Function that bridges modules.
    birdgeFun = function() {
      return(private$.bridgeFun)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method.
    #'
    #' @param ... (`ShinyModule`) ShinyModules to bridge.
    #' @param bridgeFun (`function`: `NULL`) Server function to make the
    #' modules interact with eachother. Should be setup as a shiny server
    #' function that takes `input`, `output`, and `session` as parameters.
    #'
    #' @returns `self`
    initialize = function(..., bridgeFun = NULL) {
      super$initialize()
      private$.modules <- list(...)

      if (is.null(bridgeFun)) {
        private$.bridgeFun <- function(input, output, session) {}
      } else {
        private$.bridgeFun <- bridgeFun
      }

      for (module in private$.modules) {
        module$parentNamespace <- self$namespace
      }
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .modules = NULL,
    .bridgeFun = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        lapply(private$.modules, function(module) {
          module$UI()
        })
      )
    },

    .server = function(input, output, session) {
      for (module in private$.modules) {
        module$server(input, output, session)
      }
      do.call(
        what = private$.bridgeFun,
        args = list(input = input, output = output, session = session)
      )
    }
  )
)
