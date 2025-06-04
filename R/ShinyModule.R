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

#' @title Module Decorator Class
#'
#' @description
#' This class is a `decorator` and is not meant to be directly used, but to be
#' inherited by other Modules.
#'
#' @details
#' **Namespacing**\cr
#' The `ShinyModule` class manages namespacing with the `moduleName` and
#' `instanceId`, to create a `moduleId`. The `moduleId` and `parentNamespace`
#' (when a module is nested in another module) make up the `namespace` field.
#'
#' \preformatted{
#'   moduleId = moduleName-instanceId
#'   namespace = [parentNamespace-]moduleId
#' }
#'
#' **Server method**\cr
#' When creating a new module, the `id` in `shiny::moduleServer()` is set to
#' the `moduleId` field.
#'
#' Besides setting the `id`, the `initServer()` method is called at the start of
#' `shiny::moduleServer()`. This initializes a reactive environment to be used
#' freely when developing modules. This method may be expanded upon to
#' initialize other namespace dependant features.
#'
#' All of this is done by the class it self, in the public `server()` method.
#' The `server()` method calls the private `private$.server()`, which should
#' overridden when creating a module.
#'
#' As an example:
#' \preformatted{
#' ...
#' private = list(
#'   .server = function(input, output, session) {
#'     output$table <- shiny::renderTable(iris)
#'   }
#' )
#' ...
#' }
#'
#' If the public `server()` method is overridden an error will be thrown:
#' \preformatted{
#' ...
#' public = list(
#'   server = function(input, output, session) {
#'     output$table <- shiny::renderTable(iris)
#'   }
#' )
#' ...
#'
#' myModule <- MyModule$new()
#' #> `self$server()` was overridden in `public = list(...)` override `private$.server()` instead in `private = list(.server = function(input, output, session) {})`
#' }
#'
#' **UI method**\cr
#' When accessing an `outputId` in the UI, the `namespace` field is used to
#' reference the correct namespace with `shiny::NS()`.
#'
#' It is also expected that the `UI()` method returns all contents to be shown,
#' so if multiple things should be shown, they should be nested in, as an
#' exmaple, `shiny::taglist()`.
#'
#' As an example:
#' \preformatted{
#' ...
#' private = list(
#'   .UI = function() {
#'     # `private$.namespace` would also be valid.
#'     shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
#'   }
#' )
#' }
#'
#' If the public `UI()` method is overridden an error will be thrown:
#' \preformatted{
#' ...
#' public = list(
#'   server = function(input, output, session) {
#'     output$table <- shiny::renderTable(iris)
#'   }
#' )
#' ...
#'
#' myModule <- MyModule$new()
#' #> `self$UI()` was overridden in `public = list(...)` override `private$.UI()` instead in `private = list(.UI = function() {})`
#' }
#'
#' @export
#'
#' @examples
#' MyModule <- R6::R6Class(
#'   classname = "MyModule",
#'   inherit = ShinyModule,
#'   private = list(
#'     .UI = function() {
#'       # `private$.namespace` would also be valid.
#'       shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
#'     },
#'
#'     # Override server()
#'     .server = function(input, output, session) {
#'       output$table <- shiny::renderTable(iris)
#'     }
#'   )
#' )
#'
#' if (interactive()) {
#'   myModule <- MyModule$new()
#'   preview(myModule)
#' }
#'
#' # The following would throw an error for overwritnig the public UI() and server() methods:
#' MyModule <- R6::R6Class(
#'   classname = "MyModule",
#'   inherit = ShinyModule,
#'   public = list(
#'     UI = function() {
#'       # `private$.namespace` would also be valid.
#'       shiny::tableOutput(outputId = shiny::NS(self$namespace, "table"))
#'     },
#'
#'     # Override server()
#'     server = function(input, output, session) {
#'       output$table <- shiny::renderTable(iris)
#'     }
#'   )
#' )
#'
#' tryCatch(
#'   {
#'     myModule <- MyModule$new()
#'   },
#'   error = function(e) {
#'     message(e)
#'   }
#' )
#' #> `self$server()` was overridden in `public = list(...)` override
#' #> `private$.server()` instead in
#' #> `private = list(.server = function(input,output, session) {})`
#'
#' #> `self$UI()` was overridden in `public = list(...)` override
#' #> `private$.UI()` instead in `private = list(.UI = function() {})`
ShinyModule <- R6::R6Class(
  classname = "ShinyModule",

  # Active ----
  active = list(
    #' @field instanceId (`character(1)`) Random ID of 10 capitalized letters.
    instanceId = function(instanceId) {
      if (missing(instanceId)) {
        return(private$.instanceId)
      } else {
        checkmate::assertCharacter(x = instanceId, len = 1)
        private$.instanceId <- instanceId
        private$.moduleId <- paste(c(private$.moduleName, private$.instanceId), collapse = "-")
        private$.namespace <- paste(c(private$.parentNamespace, private$.moduleId), collapse = "-")
        return(invisible(self))
      }
    },

    #' @field parentNamespace (`character(1)`) Namespace of the parent module.
    parentNamespace = function(parentNamespace) {
      if (missing(parentNamespace)) {
        return(private$.parentNamespace)
      } else {
        checkmate::assertCharacter(x = parentNamespace, len = 1, null.ok = TRUE)
        private$.parentNamespace <- parentNamespace
        private$.namespace <- paste(c(private$.parentNamespace, private$.moduleId), collapse = "-")
        return(invisible(self))
      }
    },

    #' @field moduleName (`character(1)`) Name of the module.
    moduleName = function() {
      return(private$.moduleName)
    },

    #' @field moduleId (`character(1)`) Module identifier, composed like:
    #' `moduleName-instanceId`
    moduleId = function() {
      return(private$.moduleId)
    },

    #' @field namespace (`character(1)`) Namespace, composed like:
    #' `[parentNamespace-]moduleName-instanceId` where `parentNamespace` is
    #' optional
    namespace = function() {
      return(private$.namespace)
    },

    #' @field reactiveValues (`reactivevalues`) Reactive values. use
    #' `shiny::isolate()` to get a non-reactive item from the reactive
    #' environment.
    reactiveValues = function() {
      return(private$.reactiveValues)
    },

    #' @field async (`logical(1)`: `FALSE`) Logical parameter to switch
    #' asynchronous mode on or off.
    async = function(async) {
      if (missing(async)) {
        return(private$.async)
      } else {
        checkmate::assertLogical(x = async, len = 1)
        private$.async <- async
      }
    }
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @return
    #' (`self`)
    initialize = function() {
      private$checkMethodOverrides()
      private$.moduleName <- class(self)[1]
      private$.instanceId <- private$makeInstanceId()
      private$.moduleId <- sprintf("%s-%s", private$.moduleName, private$.instanceId)
      private$.namespace <- c(private$.parentNamespace, private$.moduleId)
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "instanceId",
        x = private$.instanceId,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "parentNamespace",
        x = private$.parentNamespace,
        len = 1,
        null.ok = TRUE,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleName",
        x = private$.moduleName,
        len = 1,
        add = assertions
      )

      checkmate::assertCharacter(
        .var.name = "moduleId",
        x = private$.moduleId,
        len = 1,
        add = assertions
      )

      checkmate::reportAssertions(assertions)
      return(invisible(self))
    },

    #' @description
    #' Method to include a \link[shiny]{tagList} to include the body.
    #'
    #' @return
    #' (`tagList`)
    UI = function() {
      private$.UI()
    },

    #' @description
    #' Method to handle the back-end.
    #'
    #' @template param_input
    #' @template param_output
    #' @template param_session
    #'
    #' @return
    #' (`NULL`)
    server = function(input, output, session) {
      shiny::moduleServer(id = self$moduleId, module = function(input, output, session) {
        private$.init()
        if (private$.async) {
          promises::future_promise(private$.server(input, output, session))
        } else {
          private$.server(input, output, session)
        }
      })
      return(NULL)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .moduleName = "",
    .instanceId = "",
    .moduleId = "",
    .parentNamespace = NULL,
    .namespace = "",
    .reactiveValues = NULL,
    .async = FALSE,

    ## Methods ----
    .init = function() {
      private$.reactiveValues <- shiny::reactiveValues()
      return(invisible(self))
    },
    .server = function(input, output, session) {},
    .UI = function(input, output, session) {},
    assertInstall = function(pkgName, version) {
      if (!require(pkgName, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE) ||
        packageVersion(pkgName) < version) {
        answer <- readline(prompt = sprintf("`%s` >= %s is not installed, would you like to install from CRAN? (y/n)", pkgName, version))
        if (substr(tolower(answer), start = 1, stop = 1) == "y") {
          utils::install.packages(pkgName)
        } else {
          stop("Your answer was not `y` or `n`")
        }
      }
      return(invisible(NULL))
    },
    finalize = function() {
      return(NULL)
    },
    makeInstanceId = function(n = 20) {
      items <- c(letters, LETTERS, c(1:9), c("_"))
      paste0(sample(x = items, size = n), collapse = "")
    },
    checkMethodOverrides = function() {
      if (!is.null(self$.__enclos_env__$super)) {
        serverErr <- if (!identical(self$.__enclos_env__$super$server, self$server)) {
          "`self$server()` was overridden in `public = list(...)` override `private$.server()` instead in `private = list(.server = function(input, output, session) {})`"
        }

        uiErr <- if (!identical(self$.__enclos_env__$super$UI, self$UI)) {
          "`self$UI()` was overridden in `public = list(...)` override `private$.UI()` instead in `private = list(.UI = function() {})`"
        }

        if (any(!is.null(c(serverErr, uiErr)))) {
          stop(c(serverErr, "\n  ", uiErr))
        }
      }
    }
  )
)
