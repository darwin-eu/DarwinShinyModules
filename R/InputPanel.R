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
    args = function(args) {
      if (missing(args)) {
        return(private$.args)
      } else {
        private$.args <- args
      }
    },

    #' @field inputValues (`reactiveValues`) Values passed from the input fields.
    inputValues = function(inputValues) {
      if (missing(inputValues)) {
        return(self$getReactiveValues())
      } else {
        private$.reactiveValues <- inputValues
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
      checkmate::assertChoice(x = growDirection, choices = c("vertical", "horizontal"))
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
    },

    #' @description
    #' Updates the input variables using the provided update functions supplied in `updateFuns`
    #'
    #' @param fun (`funciton`) Update function to use i.e. `shiny::updateSelectInput`
    #' @param name (`character(1)`) Name of the update function and argument set to use.
    #' @param ... Arguments that are used by the supplied function. `inputId` should now be provided, as it is derived from the `name` argument.
    #'
    #' @return (`invisible(self)`)
    update = function(fun, name, ...) {
      dots <- list(...)
      args <- append(dots, list(inputId = shiny::NS(self$moduleId, name)))
      do.call(fun, args)
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
        result <- shiny::fluidPage(
          lapply(names(private$.funs), function(name) {
            shiny::div(
              style = "display: inline-block;",
              do.call(what = private$.funs[[name]], args = private$.args[[name]])
            )
          })
        )
      } else {
        result <- shiny::fluidPage(
          lapply(names(private$.funs), function(name) {
            do.call(what = private$.funs[[name]], args = private$.args[[name]])
          })
        )
      }
      return(result)
    },

    .server = function(input, output, session) {
      lapply(names(private$.args), function(label) {
        # Expression added to the observer, to track value changes. Evaluated later.
        shiny::observe({
          private$.reactiveValues[[session$token]][[label]] <- input[[label]]
        })

        # Expression evaluated immediately on start up, to initialize the first selected value
        private$.reactiveValues[[session$token]][[label]] <- shiny::isolate(input[[label]])
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
