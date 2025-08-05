renameDatabases <- function(cdmSourceInfo) {
  dupes <- cdmSourceInfo |>
    dplyr::group_by(.data$cdm_source_abbreviation) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::pull(.data$cdm_source_abbreviation)

  if (length(dupes)) {
    cdmSourceInfo <- cdmSourceInfo |>
      dplyr::group_by(.data$cdm_source_abbreviation) |>
      dplyr::mutate(
        cdm_source_abbreviation = dplyr::case_when(
          .data$cdm_source_abbreviation %in% dupes ~ paste0(.data$cdm_source_abbreviation, " - Analysis: ", .data$analysis_id),
          .default = .data$cdm_source_abbreviation
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::collect()
  }
  return(cdmSourceInfo)
}
