# Copyright 2026 DARWIN EU®
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

#' @title Flextable Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Flextable module that displays tables using `flextable` that are displayed by
#' `renderUI` and `uiOutput`.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' gtTable <- Flextable$new(
#'   fun = flextable::flextable,
#'   args = list(data = iris)
#' )
#'
#' if (interactive()) {
#'   preview(gtTable)
#' }
Flextable <- R6::R6Class(
  classname = "Flextable",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    #' @field fun (`function`) Function to produce a `flextable` table with, i.e `flextable::flextable`.
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
    #' @param fun (`function`) Function to produce a `flextable` table with, i.e `flextable::flextable`.
    #' @param args (`list()`) Arguments for said function as a named list i.e. `list(data = iris)`.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(fun, args, ...) {
      super$initialize(...)
      private$assertFlextableInstall()
      private$.fun <- fun
      private$.args <- args
      private$.dots <- list(...)
      private$.dots <- private$.dots[!names(private$.dots) %in% c("parentNamespace", "async")]
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    .fun = NULL,
    .args = NULL,
    .dots = list(),

    .UI = function() {
      shiny::tagList(
        do.call(
          what = shiny::uiOutput,
          args = append(
            list(outputId = shiny::NS(private$.namespace, "FlexTable")),
            private$.dots
          )
        ),
        shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlButton"), label = "docx")
      )
    },

    .server = function(input, output, session) {
      output$FlexTable <- shiny::renderUI({
        flextable::htmltools_value(do.call(private$.fun, private$.args))
      })
      private$downloader(output)
    },

    assertFlextableInstall = function() {
      if (!require("flextable", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)) {
        stop("Required package: `flextable` is not installed")
      }
    },

    downloader = function(output) {
      output$dlButton <- shiny::downloadHandler(
        filename = private$dlFilename,
        content = private$dlContent
      )
    },

    dlFilename = function() {
      return("flextable.docx")
    },

    dlContent = function(file) {
      do.call(private$.fun, private$.args) |>
        flextable::save_as_docx(path = file)
    }
  )
)
