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

#' @title DatabaseOverview Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' The DatabaseOverview module displays information about data partners, and
#' links to the Portal and data partner website.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' databaseOverview <- DatabaseOverview$new("IPCI", "CPRD GOLD", "SIDIAP")
#'
#' if (interactive()) {
#'   preview(databaseOverview)
#' }
DatabaseOverview <- R6::R6Class(
  classname = "DatabaseOverview",
  inherit = ShinyModule,

  active = list(
    #' @field table (`Table`) Table module
    table = function() {
      return(private$.table)
    }
  ),

  public = list(
    #' @description Initializer method
    #'
    #' @param ... (`character(n)`) Individual database names. See Details for
    #' the supported data partners.
    #'
    #' @details
    #' \describe{
    #'   \item{A}{"APHM"}
    #'   \item{B}{"BIFAP"}
    #'   \item{C}{"CDW Bordeaux"\cr"CPRD GOLD" "CRN"}
    #'   \item{D}{"DK-DHR"}
    #'   \item{E}{"EBB"\cr"EMDB - ULSEDV"\cr"EMDB - ULSGE"\cr"EMDB - ULSRA"}
    #'   \item{F}{"FinOMOP - ACI Varha"\cr"FinOMOP - HILMO"\cr"FinOMOP - HUS"\cr"FinOMOP - TaUH Pirha"}
    #'   \item{H}{"H12O"\cr"HARMONY Platform"\cr"HARMONY - ALL"\cr"HARMONY - AML"\cr"HARMONY - CML"\cr"HARMONY - MM"}
    #'   \item{I}{"IMASIS"\cr"InGef RDB"\cr"IPCI"\cr"IQVIA DA Germany"\cr"IQVIA LPD Belgium"}
    #'   \item{N}{"NAJS"\cr"NCR"\cr"NLHR"\cr"NLHR@UiO:PERINATAL"\cr"NNRD"}
    #'   \item{P}{"PGH"\cr"PRISIB"\cr"POLIMI"}
    #'   \item{S}{"SIDIAP"\cr"SNDS"\cr"SUCD"}
    #'   \item{U}{"UKBB"\cr"ULSM-RT"}
    #'   \item{V}{"VID"}
    #' }
    initialize = function(...) {
      dots <- c(...)
      dps <- readRDS(system.file(package = "DarwinShinyModules", "datapartners.RDS"))
      dpChoices <- unique(dps$DB_name)
      dots <- sapply(dots, checkmate::assertChoice, choices = dpChoices, .var.name = "datapartners")

      data <- dps %>%
        dplyr::filter(.data$DB_name %in% dots) %>%
        dplyr::mutate(
          `Database Name` = dplyr::case_when(.data$QuestionNumber == "1.02" ~ .data$Answer),
          Acronym = dplyr::case_when(.data$QuestionNumber == "1.01" ~ .data$Answer),
          Institution = dplyr::case_when(.data$QuestionNumber == "1.03" ~ .data$Answer),
          Description = dplyr::case_when(.data$QuestionNumber == "1.06" ~ .data$Answer),
          Website = dplyr::case_when(.data$QuestionNumber == "1.05" ~ .data$Answer),
          Portal = sprintf("https://portal.darwin-eu.org/c/DARWIN/fingerprint/%s/", .data$DB_ID),
        ) %>%
        dplyr::group_by(.data$DB_ID) %>%
        dplyr::reframe(across(everything(), ~ na.omit(.x))) %>%
        dplyr::select(-"DB_ID", -"DB_name", -"QuestionNumber", -"Answer", -"field") %>%
        distinct()

      private$.table <- GTTable$new(fun = private$fun, args = list(data = data))
      private$.table$parentNamespace <- self$namespace
    }
  ),

  private = list(
    ## Fields ----
    .table = NULL,

    ## Methods ----
    .UI = function() {
      private$.table$UI()
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
    },

    fun = function(data) {
      data %>%
        dplyr::mutate(
          Portal = purrr::map(.data$Portal, ~ htmltools::a(href = .x, "Portal")),
          Portal = purrr::map(.data$Portal, ~ gt::html(as.character(.x))),
          Website = purrr::map(.data$Website, ~ htmltools::a(href = .x, "Website")),
          Website = purrr::map(.data$Website, ~ gt::html(as.character(.x)))
        ) %>%
        gt::gt(groupname_col = "Database Name") %>%
        gt::tab_spanner(
          label = "Links",
          columns = c(Portal, Website)
        ) %>%
        gt::tab_footnote(
          footnote = gt::md("*Data source description*")
        ) %>%
        gt::tab_options(column_labels.font.weight = "bold") %>%
        gt::opt_interactive(use_compact_mode = TRUE) %>%
        gt::cols_width(
          Description ~ gt::px(800)
        )
    }
  )
)
