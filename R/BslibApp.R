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

BslibApp <- R6::R6Class(
  classname = "BslibApp",
  inherit = App,

  ## Public ----
  public = list(
    ## Methods ----
    initialize = function(appStructure, title = NULL) {
      super$initialize(appStructure)
      private$.title <- title
    },

    UI = function() {
      bslib::page(
        do.call(bslib::navset_bar, private$parseModules())
      )
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .title = NULL,

    ### Methods ----
    parseModules = function() {
      lapply(seq_len(length(private$.appStructure)), function(i) {
        menuName <- names(private$.appStructure[i])
        menu <- private$.appStructure[[i]]
        if ("ShinyModule" %in% class(menu)) {
          bslib::nav_panel(
            title = menuName,
            menu$UI()
          )
        } else if (is.null(names(menu))) {
          bslib::nav_panel(
            title = menuName,
            lapply(menu, function(module) {
              module$UI()
            })
          )
        } else {
          panelItems <- lapply(seq_len(length(menu)), function(j) {
            panelName <- names(menu[j])
            panel <- menu[[j]]
            bslib::nav_panel(
              title = panelName,
              if (all(class(panel) == "list")) {
                lapply(panel, function(module) {
                  module$UI()
                })
              } else {
                panel$UI()
              }
            )
          })
          do.call(bslib::nav_menu, args = c(title = menuName, panelItems))
        }
      })
    }
  )
)
