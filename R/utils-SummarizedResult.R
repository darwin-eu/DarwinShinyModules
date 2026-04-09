getCDMNames <- function(result) {
  UseMethod("getCDMNames")
}

getCohortNames <- function(result) {
  UseMethod("getCohortNames")
}

getStrata <- function(result) {
  UseMethod("getStrata")
}

availableTableColumns <- function(result) {
  UseMethod("availableTableColumns")
}

availablePlotColumns <- function(result) {
  UseMethod("availablePlotColumns")
}

getCDMNames.summarised_result <- function(result) {
  result |>
    dplyr::distinct(.data$cdm_name) |>
    dplyr::pull(.data$cdm_name)
}

getCohortNames.summarised_result <- function(result) {
  result |>
    dplyr::filter(.data$group_name == "cohort_name") |>
    dplyr::distinct(.data$group_level) |>
    dplyr::pull(.data$group_level)
}

getStrata.summarised_result <- function(result) {
  result |>
    dplyr::filter(!.data$strata_name %in% c("reason", "overall")) |>
    dplyr::distinct(.data$strata_name) |>
    dplyr::pull(.data$strata_name)
}

availableTableColumns.summarised_result <- function(result) {
  result <- omopgenerics::validateResultArgument(result)
  c("cdm_name", omopgenerics::groupColumns(result), omopgenerics::strataColumns(result),
    omopgenerics::additionalColumns(result), omopgenerics::settingsColumns(result))
}

availablePlotColumns.summarised_result <- function(result) {
  result <- omopgenerics::validateResultArgument(result)
  c("cdm_name", omopgenerics::groupColumns(result), omopgenerics::strataColumns(result),
    omopgenerics::additionalColumns(result), omopgenerics::settingsColumns(result),
    unique(result$estimate_name))
}
