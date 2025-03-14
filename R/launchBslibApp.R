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

#' launchBslibApp
#'
#' @param appStructure (`list(list())`) A list of named lists, containing modules.
#' The level of nesting groups or separates modules in menu items `"_"` will be read as a space.
#' @param title (`character(1)`: `NULL`) Title of the app
#' @param async (`logical(1)`: `FALSE`) Run app asynchronously
#'
#' @returns `NULL`
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
#' if (interactive()) {
#'   appStructure <- list(
#'     base = base,
#'     nested = list(nested_a, nested_b),
#'     nested_sub = list(
#'       sub_a = sub_a,
#'       sub_b = sub_b
#'     ),
#'     nested_combined = list(
#'       comb_a_b = list(comb_a, comb_b),
#'       comb_c = comb_c
#'     )
#'   )
#'
#'   launchBslibApp(appStructure)
#' }
launchBslibApp <- function(appStructure, title = NULL, async = FALSE) {
  if (!rlang::is_installed(pkg = "bslib")) {
    rlang::abort("`bslib` is not installed.")
  }
  assertAppStructure(appStructure)
  checkmate::assertCharacter(title, len = 1, null.ok = TRUE)
  if (async) setAsync(appStructure)
  app <- BslibApp$new(appStructure, title = title)
  app$launch()
}
