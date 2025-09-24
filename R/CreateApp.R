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

#' Function that creates a stand-alone shiny app from a given app structure.
#'
#' @param path path where shiny app will be stored
#' @param appStructure application structure as a list
#' @param theme theme as a character, it can be one of
#' 'shinymodules' (default), 'bslib', 'bslib-darwin' or 'shinymodules-darwin'
#' @param additionalFiles optional vector of files to be copied
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' base <- Text$new("**base**")
#' nested_a <- Text$new("**nested A**")
#' nested_b <- Text$new("**nested B**")
#' sub_a <- Text$new("**sub A**")
#' sub_b <- Text$new("**sub B**")
#' comb_a <- Text$new("**comb A**")
#' comb_b <- Text$new("**comb B**")
#' comb_c <- Text$new("**comb C**")
#'
#' appStructure <- list(
#'    base = base,
#'    nested = list(nested_a, nested_b),
#'    nested_sub = list(
#'      sub_a = sub_a,
#'      sub_b = sub_b
#'    ),
#'    nested_combined = list(
#'      comb_a_b = list(comb_a, comb_b),
#'      comb_c = comb_c
#'    )
#'  )
#'
#' if (interactive()) {
#'   createApp(path = tempdir(),
#'             appStructure = appStructure,
#'             theme = 'shinymodules-darwin')
#' }
createApp <- function(path, appStructure, theme = "shinymodules", additionalFiles = c()) {
  checkmate::assertChoice(x = theme, c("bslib", "shinymodules", "bslib-darwin", "shinymodules-darwin"))

  if (!dir.exists(path)) {
    dir.create(path, recursive = T)
  }
  usethis::create_project(path = path, open = FALSE)

  themeFun <- switch(
    theme,
    "bslib" = "DarwinShinyModules::launchBslibApp(appStructure)",
    "shinymodules" = "DarwinShinyModules::launchShinydashboardApp(appStructure)",
    "bslib-darwin" = "DarwinShinyModules::launchDarwinBslibApp(appStructure)",
    "shinymodules-darwin" = "DarwinShinyModules::launchDarwinDashboardApp(appStructure)"
  )

  appStructureFile <- "appStructure.rds"
  saveRDS(appStructure, file.path(path, appStructureFile))

  writeLines(
    c("library(DarwinShinyModules)",
      "sapply(list.files('R', full.names = T), source)",
      sprintf("appStructure <- readRDS('./%s')", appStructureFile),
      themeFun),
    file.path(path, "app.R")
  )

  for (file in additionalFiles) (
    file.copy(from = file, to = file.path(path, "R", basename(file)), overwrite = TRUE)
  )
}
