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

#' @title Text
#'
#' @include ShinyModule.R
#'
#' @description
#' Text Module
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' md <- c(
#'   "# H1
#'   ## H2
#'   ### H3",
#'   "**bold text**",
#'   "*italicized text*",
#'   "> blockquote",
#'   "1. First item
#'   2. Second item
#'   3. Third item",
#'   "- First item
#'   - Second item
#'   - Third item",
#'   "`code`",
#'   "---",
#'   "[link](https://www.markdownguide.org/cheat-sheet/)",
#'   "![alt text](https://mdg.imgix.net/assets/images/
#'    san-juan-mountains.jpg?auto=format&fit=clip&q=40&w=1080)",
#'   "| Syntax | Description |
#'   | ----------- | ----------- |
#'   | Header | Title |
#'   | Paragraph | Text |",
#'   "```r
#'     foo <- function(bar, baz) {
#'       return(bar ** baz)
#'     }
#'
#'     foo(2, 3)
#'   ```",
#'   "	Here's a sentence with a footnote. [^1]
#'
#'   [^1]: This is the footnote.",
#'   "### My Great Heading {#custom-id}",
#'   "term
#'   : definition",
#'   "~~The world is flat.~~",
#'   "- [x] Write the press release
#'   - [ ] Update the website
#'   - [ ] Contact the media",
#'   "That is so funny! :joy:",
#'   "I need to highlight these ==very important words==.",
#'   "H~2~O",
#'   "X^2^"
#' )
#'
#' text <- Text$new(markdown = md)
#'
#' if (interactive()) {
#'   preview(module = text)
#' }
Text <- R6::R6Class(
  classname = "Text",
  inherit = ShinyModule,

  # Active ----
  active = list(
    #' @field markdown (`character(n)`) Lines read from the markdown-file supplied.
    markdown = function() {
      return(private$.markdown)
    }
  ),

  # PUblic ----
  public = list(
    ## Methods ----

    #' @description initialize
    #'
    #' @param markdown (`character(n)`) Markdown.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(markdown, ...) {
      super$initialize(...)
      private$.markdown <- markdown
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return
    #' (`self`)
    validate = function() {
      super$validate()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(
        .var.name = "markdown",
        x = private$.markdown,
        add = assertions
      )
      checkmate::reportAssertions(assertions)
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .markdown = "",

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::markdown(private$.markdown)
      )
    }
  )
)
