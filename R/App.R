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

App <- R6::R6Class(
  classname = "App",

  active = list(),
  ## Public ----
  public = list(
    ## Methods ----
    initialize = function(appStructure) {
      private$.appStructure <- appStructure
    },

    launch = function() {
      shiny::shinyApp(ui = self$UI(), server = self$server)
    },

    server = function(input, output, session) {
      modules <- unlist(private$.appStructure)
      for (module in modules) {
        module$server(input, output, session)
      }
    },

    UI = function() {}
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .appStructure = list()
  )
)
