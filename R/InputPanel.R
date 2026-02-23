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

#' @title InputPanel Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' InputPanel module that handles inputs based on an input function like:
#' `shiny::selectInput()`, `shinyWidgets::pickerInput()`, etc.
#'
#' @details
#' The assigned input values are accessible in the reactive values
#' `inputValues` field. Other modules may trigger off these reactive values
#' with i.e. `shiny::observeEvent()`.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' inputPanel <- InputPanel$new(
#'   funs = list(
#'     select = shiny::selectInput,
#'     text = shiny::textInput
#'   ),
#'   args = list(
#'     select = list(inputId = "select", choices = c("a", "b"), label = "select"),
#'     text = list(inputId = "text", label = "text")
#'   )
#' )
#'
#' if (interactive()) {
#'   preview(inputPanel)
#' }
InputPanel <- R6::R6Class(
  classname = "InputPanel",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field parentNamespace (`character(1)`) Namespace of the parent module.
    parentNamespace = function(parentNamespace) {
      super$parentNamespace <- parentNamespace
      private$updateIds()
      return(invisible(self))
    },

    #' @field funs (`list()`) Named list of xInput functions used `list(funA = shiny::selectInput)`.
    funs = function() {
      return(private$.funs)
    },

    #' @field args (`list()`) Named list of arguments used by xInput functions `list(funA = list(inputId = "name", label = "name"))`.
    args = function() {
      return(private$.args)
    },

    #' @field inputValues (`reactiveValues`) Values passed from the input fields.
    inputValues = function(inputValues) {
      if (missing(inputValues)) {
        return(self$getReactiveValues())
      } else {
        self$reactiveValues <- inputValues
      }
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @param funs (`list()`) Named list of xInput functions used `list(funA = shiny::selectInput)`.
    #' @param args (`list()`) Named list of arguments used by xInput functions `list(funA = list(inputId = "name", label = "name"))`
    #' @param growDirection The direction in which this component will be placed, either "horizontal" or "vertical" (default)
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return (`invisible(self)`)
    initialize = function(funs, args, growDirection = "vertical", ...) {
      super$initialize(...)
      private$.funs <- funs
      private$.args <- args
      private$.growDirection <- growDirection
      private$updateIds()
      self$validate()
      return(invisible(self))
    },

    #' @description
    #' Validation method
    #'
    #' @return (`self`)
    validate = function() {
      return(invisible(self))
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .funs = NULL,
    .args = NULL,
    .growDirection = "vertical",
    .UI = function() {
      if (private$.growDirection == "horizontal") {
        result <- shiny::tagList(
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            lapply(names(private$.funs), function(name) {
              do.call(what = private$.funs[[name]], args = private$.args[[name]])
            })
          )
        )
      } else {
        result <- shiny::tagList(
          lapply(names(private$.funs), function(name) {
            do.call(what = private$.funs[[name]], args = private$.args[[name]])
          })
        )
      }
      return(result)
    },
    .server = function(input, output, session) {
      lapply(names(private$.args), function(label) {
        shiny::observeEvent(input[[label]],
          {
            private$.reactiveValues[[session$token]][[label]] <- input[[label]]
          },
          ignoreNULL = FALSE
        )
      })
    },
    updateIds = function() {
      for (name in names(private$.args)) {
        if (!is.null(private$.args[[name]]$inputId)) {
          private$.args[[name]]$inputId <- shiny::NS(private$.namespace, name)
        }
      }
    }
  )
)
