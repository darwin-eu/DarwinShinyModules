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
    saveRDS(file = "./inst/datapartners.RDS")
  return(invisible(NULL))
}
