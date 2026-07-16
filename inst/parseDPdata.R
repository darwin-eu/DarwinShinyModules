parseDPdata <- function(path) {
  df <- read.csv(path, sep = "\t")

  df |>
    dplyr::filter(.data$QuestionNumber %in% c("1.01", "1.02", "1.03", "1.05", "1.06")) |>
    dplyr::group_by(.data$DB_ID, .data$DB_name, .data$QuestionNumber, .data$Answer) |>
    dplyr::reframe() |>
    dplyr::mutate(
      field = dplyr::case_when(
        .data$QuestionNumber == "1.01" ~ "db_acrynym",
        .data$QuestionNumber == "1.02" ~ "db_Name",
        .data$QuestionNumber == "1.03" ~ "institution",
        .data$QuestionNumber == "1.05" ~ "website",
        .data$QuestionNumber == "1.06" ~ "db_description",
        .default = ""
      )
    ) |>
    dplyr::mutate(DB_name = trimws(.data$DB_name)) |>
    dplyr::mutate(DB_name_multi_line = dplyr::case_when(
      grepl(pattern = "IQVIA ", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "IQVIA ", replacement = "IQVIA\\\n"),
      grepl(pattern = "FinOMOP-", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "FinOMOP-", replacement = "FinOMOP-\\\n"),
      grepl(pattern = "HARMONY ", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "HARMONY ", replacement = "HARMONY\\\n"),
      grepl(pattern = "HARMONY-", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "HARMONY-", replacement = "HARMONY-\\\n"),
      grepl(pattern = "CPRD ", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "CPRD ", replacement = "CPRD\\\n"),
      grepl(pattern = "EMDB-", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "EMDB-", replacement = "EMDB-\\\n"),
      grepl(pattern = "CDW Bordeaux", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "CDW Bordeaux", replacement = "CDW\\\nBordeaux"),
      grepl(pattern = "NLHR@UiO:PERINATAL", x = .data$DB_name) ~ stringr::str_replace(string = .data$DB_name, pattern = "NLHR@UiO:PERINATAL", replacement = "NLHR@UiO:\\\nPERINATAL"),
      .default = .data$DB_name
    )) |>
    saveRDS(file = "./inst/datapartners.RDS")
  return(invisible(NULL))
}
