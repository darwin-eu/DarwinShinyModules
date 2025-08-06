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

#' @title Plot Decorator Class
#'
#' @include ShinyModule.R
#'
#' @description
#' This class is a `decorator` and is not meant to be directly used, but to be
#' inherited by other modules, like `PlotStaic`, `PlotWidget`, and `PlotPlotly`.
#'
#' @details
#' The inherited `Plot` modules evaluate the provided function with a provided
#' data object.
#'
#' To add a new plot type it is required to inherit from the `Plot` class, and
#' to override the private `.UI()` and `.server()` methods.
#'
#' @export
Plot <- R6::R6Class(
  classname = "Plot",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field title (`character(1)`) Title to use for the plot.
    title = function(title) {
      if (missing(title)) {
        return(private$.title)
      } else {
        checkmate::assertCharacter(title, len = 1)
        private$.title <- title
      }
      return(invisible(self))
    },

    #' @field fun Plotting function.
    fun = function(fun) {
      if (missing(fun)) {
        return(private$.fun)
      } else {
        checkmate::assertFunction(fun)
        private$.fun <- fun
      }
    },

    #' @field args (`reactiveValues`) Arguments used for plot.
    args = function(args) {
      if (missing(args)) {
        return(private$.args)
      } else {
        private$.args <- args
      }
    },

    #' @field plot Plot object.
    plot = function() {
      return(private$.plot)
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description initialize
    #'
    #' @param fun (`function()`) Function to plot with.
    #' @param args (`list`) Named list of arguments to pass to `fun`.
    #' @param title (`character(1)`) Title of the plot. When set to `NULL`, no title is shown.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(fun, args, title = "Plot", ...) {
      super$initialize(...)
      private$.fun <- fun
      private$.args <- args
      private$.title <- title
      self$validate()
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::reportAssertions(assertions)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .fun = NULL,
    .args = NULL,
    .title = "",
    .data = NULL,
    .plot = NULL,

    ## Methods ----
    .server = function(input, output, session) {
      if (!shiny::is.reactivevalues(private$.args)) {
        private$.args <- do.call(shiny::reactiveValues, private$.args)
      }
      shiny::onStop(fun = private$finalize)
    },
    finalize = function() {
      self$args <- isolate(shiny::reactiveValuesToList(self$args))
    }
  )
)
