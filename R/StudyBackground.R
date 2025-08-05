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

#' @title StudyBackground
#'
#' @include ShinyModule.R
#'
#' @description
#' StudyBackground Module that contains background information and the EUPAS.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' studyBackground <- StudyBackground$new(
#'   background = "./background.md",
#'   EUPAS = "EUPAS9999999"
#' )
#'
#' if (interactive()) {
#'   preview(studyBackground)
#' }
StudyBackground <- R6::R6Class(
  classname = "StudyBackground",
  inherit = ShinyModule,
  active = list(
    #' @field background (`character(n)`) Either the direct background, or the
    #' contents of a markdown (.md) file.
    background = function() {
      return(private$.background)
    },

    #' @field EUPAS (`character(1)`) EUPAS belonging to the study.
    EUPAS = function() {
      return(private$.EUPAS)
    },

    #' @field text (`Text`) A Text module.
    text = function() {
      return(private$.text)
    }
  ),
  public = list(
    #' @description
    #' initializer method
    #'
    #' @param background (`character(n)`) Either a direct background description
    #' or a file path pointing to a markdown (.md) file.
    #' @param EUPAS (`character(1)`) EUPAS belonging to the study.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `invisible(self)`
    initialize = function(background, EUPAS, ...) {
      super$initialize(...)
      private$.background <- private$parseSection(background)
      private$.EUPAS <- EUPAS

      private$.text <- Text$new(
        markdown = c(
          sprintf("**%s**", private$.EUPAS),
          "\n\n",
          "## Background",
          private$.background
        )
      )
      private$.text$parentNamespace <- self$namespace
      return(invisible(self))
    }
  ),
  private = list(
    .background = "",
    .EUPAS = "",
    .text = NULL,
    .UI = function() {
      shiny::wellPanel(
        private$.text$UI()
      )
    },
    parseSection = function(section) {
      if (all(file.exists(section))) {
        readLines(section)
      } else {
        section
      }
    }
  )
)
