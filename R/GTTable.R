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

#' @title GTTable Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' GTTable module that displays tables using `gt` that are supported by
#' `gt::render_gt()` and `gt::gt_output()`.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' gtTable <- GTTable$new(
#'   fun = gt::gt,
#'   args = list(data = iris)
#' )
#'
#' if (interactive()) {
#'   preview(gtTable)
#' }
GTTable <- R6::R6Class(
  classname = "GTTable",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field fun (`function`) Function to produce a `gt` table with, i.e `gt::gt`.
    fun = function() {
      return(private$.fun)
    },

    #' @field args (`list`) Arguments for said function as a named list i.e. `list(data = iris)`.
    args = function(args) {
      if (missing(args)) {
        return(private$.args)
      } else {
        private$.args <- args
      }
    }
  ),

  # Public ----
  public = list(

    #' @description
    #' Initializer method.
    #'
    #' @param fun (`function`) Function to produce a `gt` table with, i.e `gt::gt`.
    #' @param args (`list()`) Arguments for said function as a named list i.e. `list(data = iris)`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(fun, args, ...) {
      super$initialize(...)
      private$assertGtInstall()
      private$.fun <- fun
      private$.args <- args
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .fun = NULL,
    .args = NULL,
    .UI = function() {
      shiny::tagList(
        gt::gt_output(outputId = shiny::NS(private$.namespace, "gtTable"))
      )
    },
    .server = function(input, output, session) {
      output$gtTable <- gt::render_gt({
        do.call(private$.fun, private$.args)
      })
    },
    assertGtInstall = function() {
      if (!require("gt", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)) {
        stop("Required package: `gt` is not installed")
      }
    }
  )
)
