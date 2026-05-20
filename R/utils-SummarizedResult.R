getCDMNames <- function(result) {
  UseMethod("getCDMNames")
}

getCohortNames <- function(result) {
  UseMethod("getCohortNames")
}

checkCDMNames <- function(result, .softValidation) {
  UseMethod("checkCDMNames")
}

assertType <- function(result, type) {
  UseMethod("assertType")
}

#' @exportS3Method DarwinShinyModules::getCDMNames
getCDMNames.summarised_result <- function(result) {
  result |>
    dplyr::distinct(.data$cdm_name) |>
    dplyr::pull(.data$cdm_name)
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
    dplyr::filter(!.data$strata_name %in% c("reason")) |>
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

#' @exportS3Method DarwinShinyModules::checkCDMNames
checkCDMNames.summarised_result <- function(result, .softValidation = FALSE) {
  tar <- result |>
    dplyr::select(cdm_name_result = "cdm_name") |>
    dplyr::distinct() |>
    dplyr::pull()

  ref <- readRDS(system.file("datapartners.RDS", package = "DarwinShinyModules")) |>
    dplyr::filter(.data$field == "db_acrynym") |>
    dplyr::select("Answer") |>
    dplyr::pull()


  good <- tar[tar %in% ref]
  bad <- tar[!tar %in% ref]

  subStr <- substr(bad, start = 0, stop = 4)

  matches <- lapply(subStr, function(str) {
    res <- ref[grepl(tolower(str), tolower(ref))]
    res <- if (length(res) == 0) {
      ""
    } else {
      res
    }
    return(res)
  })

  if (length(bad) == 0) {
    return(invisible(NULL))
  } else {
    for (i in seq_len(length(bad))) {
      warning(sprintf("The following cdm_name need to be updated: `%s`. Moste likely to one of: %s\n", bad[i], paste(sprintf("`%s`", matches[[i]]), collapse = ", ")))
    }
    errMsg <- "Some acryonyms in `cdm_name` are not in the standard format. To get the full list of acronyms use: `getCDMAcronyms()`"
    if (.softValidation) {
      warning(errMsg)
    } else {
      stop(errMsg)
    }
  }
}

#' @exportS3Method DarwinShinyModules::checkCDMNames
checkCDMNames.TreatmentPatternsResults <- function(tpr, .softValidation = FALSE) {
  tar <- tpr$cdm_source_info$cdm_source_abbreviation |>
    unique()

  ref <- readRDS(system.file("datapartners.RDS", package = "DarwinShinyModules")) |>
    dplyr::filter(.data$field == "db_acrynym") |>
    dplyr::select("Answer") |>
    dplyr::pull()


  good <- tar[tar %in% ref]
  bad <- tar[!tar %in% ref]

  subStr <- substr(bad, start = 0, stop = 4)

  matches <- lapply(subStr, function(str) {
    res <- ref[grepl(tolower(str), tolower(ref))]
    res <- if (length(res) == 0) {
      ""
    } else {
      res
    }
    return(res)
  })

  if (length(bad) == 0) {
    return(invisible(NULL))
  } else {
    for (i in seq_len(length(bad))) {
      warning(sprintf("The following `cdm_source_abbreviation` need to be updated: `%s`. Moste likely to one of: %s\n", bad[i], paste(sprintf("`%s`", matches[[i]]), collapse = ", ")))
    }
    errMsg <- "Some acryonyms in `tpr$cdm_source_info$cdm_source_abbreviation` are not in the standard format. To get the full list of acronyms use: `getCDMAcronyms()`"
    if (.softValidation) {
      warning(errMsg)
    } else {
      stop(errMsg)
    }
  }
}

#' @exportS3Method DarwinShinyModules::assertType
assertType.summarised_result <- function(result, type) {
  settings <- omopgenerics::settings(result)

  if (!all(type %in% settings$result_type)) {
    realTypes <- sprintf("`%s`", settings$result_type) |>
      unique() |>
      paste(collapse = ", ")

    assertedTypes <- sprintf("`%s`", type) |>
      unique() |>
      paste(collapse = ", ")

    stop(sprintf("`result_type` is: %s and not: %s", realTypes, assertedTypes))
  } else {
    invisible(TRUE)
  }
}
