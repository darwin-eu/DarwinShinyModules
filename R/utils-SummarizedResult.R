getCDMNames <- function(result) {
  UseMethod("getCDMNames")
}

#' @exportS3Method DarwinShinyModules::getCDMNames
getCDMNames.summarised_result <- function(result) {
  result |>
    dplyr::distinct(.data$cdm_name) |>
    dplyr::pull(.data$cdm_name)
}

getCohortNames <- function(result) {
  UseMethod("getCohortNames")
}

#' @exportS3Method DarwinShinyModules::getCohortNames
getCohortNames.summarised_result <- function(result) {
  result |>
    dplyr::filter(.data$group_name == "cohort_name") |>
    dplyr::distinct(.data$group_level) |>
    dplyr::pull(.data$group_level)
}

getStrata <- function(result) {
  UseMethod("getStrata")
}

#' @exportS3Method DarwinShinyModules::getStrata
getStrata.summarised_result <- function(result) {
  result |>
    dplyr::filter(!.data$strata_name %in% c("reason", "overall")) |>
    dplyr::distinct(.data$strata_name) |>
    dplyr::pull(.data$strata_name)
}

availableTableColumns <- function(result) {
  UseMethod("availableTableColumns")
}

#' @exportS3Method DarwinShinyModules::availableTableColumns
availableTableColumns.summarised_result <- function(result) {
  result <- omopgenerics::validateResultArgument(result)
  c("cdm_name", omopgenerics::groupColumns(result), omopgenerics::strataColumns(result),
    omopgenerics::additionalColumns(result), omopgenerics::settingsColumns(result))
}

availablePlotColumns <- function(result) {
  UseMethod("availablePlotColumns")
}

#' @exportS3Method DarwinShinyModules::availablePlotColumns
availablePlotColumns.summarised_result <- function(result) {
  result <- omopgenerics::validateResultArgument(result)
  c("cdm_name", omopgenerics::groupColumns(result), omopgenerics::strataColumns(result),
    omopgenerics::additionalColumns(result), omopgenerics::settingsColumns(result),
    unique(result$estimate_name))
}
