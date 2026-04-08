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

#' @title DTTable Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' GTTable module that displays tables using `DT` that are supported by
#' `DT::renderDT()` and `DT::DTOutput()`.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' dtTable <- DTTable$new(
#'   fun = DT::datatable,
#'   args = list(data = iris)
#' )
#'
#' if (interactive()) {
#'   preview(dtTable)
#' }
DTTable <- R6::R6Class(
  classname = "DTTable",
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
      private$assertInstall("DT", as.package_version("0.0.0"))
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
        shiny::div(
          style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
          shiny::downloadButton(outputId = shiny::NS(private$.namespace, "dlButton"), label = "csv")
        ),
        shiny::div(
          style = "width: 100%;",
          do.call(
            what = DT::DTOutput,
            args = append(
              list(outputId = shiny::NS(private$.namespace, "table")),
              private$.dots
            )
          )
        )
      )
    },

    .server = function(input, output, session) {
      output$table <- DT::renderDT({
        do.call(private$.fun, private$.args)
      })
      private$downloader(output)
    },

    downloader = function(output) {
      output$dlButton <- shiny::downloadHandler(
        filename = private$dlFilename,
        content = private$dlContent
      )
    },

    dlFilename = function() {
      return("table.csv")
    },

    dlContent = function(file) {
      write.csv(isolate(self$reactiveValues$data), file)
    }
  )
)
