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

DarwinBslibApp <- R6::R6Class(
  classname = "DarwinBslibApp",
  inherit = BslibApp,

  ## Public ----
  public = list(
    UI = function() {
      footer <- darwinFooter(type = "bslib")$children[[1]]$children[[1]]

      shiny::addResourcePath(
        prefix = "www",
        directoryPath = system.file("www", package = "DarwinShinyModules")
      )

      shiny::tagList(
        shiny::tags$head(
          shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
        ),
        tags$style("
          .content-with-footer {
            padding-bottom: 80px;
          }
        "),
        darwinHeader(),
        bslib::page(
          title = private$.title,
          theme = private$theme(),
          fillable = TRUE,
          class = "content-with-footer",
          do.call(bslib::navset_bar, private$parseModules())
        ),
        shiny::tags$footer(
          style = "
            position: fixed;
            bottom: 0;
            left: 0;
            width: 100%;
            padding: 1rem;
            text-align: center;
            background: #f8f9fa;
            border-top: 1px solid #ddd;
            z-index: 1000;
            font-size: 10px;
          ",
          footer
        )
      )
    }
  ),

  ## Private ----
  private = list(
    ### Methods ----
    theme = function() {
      bslib::bs_theme(
        version = 5,
        "navbar-bg" = "#003194"
      )
    }
  )
)
