DatabaseOverview <- R6::R6Class(
  classname = "DatabaseOverview",
  inherit = ShinyModule,

  public = list(
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
          Portal = sprintf("https://portal-dev.darwin-eu.org/c/DARWIN/fingerprint/%s/", .data$DB_ID),
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
    .sourceTable = NULL,
    .table = NULL,

    .UI = function() {
      private$.table$UI()
    },

    .server = function(input, output, session) {
      private$.table$server(input, output, session)
    },

    fun = function(data) {
      gt::gt(data = data, groupname_col = "Database Name") %>%
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
