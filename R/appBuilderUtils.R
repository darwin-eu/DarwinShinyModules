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

assertAppStructure <- function(appStructure) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertList(appStructure, types = c("ShinyModule", "list"), add = assertions)
  checkmate::assertNamed(appStructure, type = "named", add = assertions)
  checkmate::reportAssertions(assertions)
}

statusCompleted <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("check"),
    badgeStatus = NULL,
    headerText = "Study Completed"
  )
}

statusOngoing <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("rotate-right"),
    badgeStatus = NULL,
    headerText = "Study Ongoing"
  )
}

statusStopped <- function() {
  shinydashboard::dropdownMenu(
    icon = shiny::icon("exclamation"),
    badgeStatus = NULL,
    headerText = "Study Stopped"
  )
}

studyStatus <- function(type) {
  switch(type,
         "stop" = statusStopped(),
         "complete" = statusCompleted(),
         "ongoing" = statusOngoing())
}

setAsync <- function(appStructure) {
  i <- 1
  while (TRUE) {
    if ("ShinyModule" %in% class(appStructure[[i]])) {
      appStructure[[i]]$async <- TRUE
    } else {
      setAsync(appStructure[[i]])
    }
    i <- i + 1
    if (i > length(appStructure)) {
      return(NULL)
    }
  }
}
