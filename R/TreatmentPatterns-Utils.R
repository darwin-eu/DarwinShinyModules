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

mergeIndividualPathways <- function(treatmentPathways, strataX, strataY) {
  # layerOneTotal <- sum(treatmentPathways$freq)
  maxLayer <- max(sapply(strsplit(treatmentPathways$pathway, split = "-"), length))

  layerColumns <- sprintf("layer_%s", 1:maxLayer)

  naReplaceList <- as.list(rep("", maxLayer))
  names(naReplaceList) <- layerColumns

  dat <- treatmentPathways |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY))) |>
    dplyr::mutate(
      total = sum(.data$freq),
      path_to_sep = .data$pathway,
      path_id = dplyr::row_number()
    ) |>
    tidyr::separate_wider_delim(
      cols = "path_to_sep",
      delim = "-",
      names = layerColumns,
      too_few = "align_start"
    ) |>
    tidyr::replace_na(naReplaceList)

  for (i in seq_len(length(layerColumns))) {
    dat <- dat |>
      dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), !!!rlang::parse_exprs(layerColumns[1:i])) |>
      dplyr::mutate(!!rlang::sym(sprintf("l%s_freq", i)) := sum(.data$freq)) |>
      dplyr::ungroup()
  }

  to0 <- as.list(rep(0, maxLayer))
  names(to0) <- sprintf("l%s_freq", 1:maxLayer)

  dat <- dat |>
    tidyr::replace_na(to0) |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY))) |>
    dplyr::arrange(!!!rlang::parse_exprs(names(to0)), !!!rlang::parse_exprs(layerColumns)) |>
    dplyr::mutate(
      frac = .data$freq / .data$total * 100,
      xmax = cumsum(.data$frac),
      xmin = .data$xmax - .data$frac
    ) |>
    tidyr::separate_longer_delim(cols = "pathway", delim = "-") |>
    dplyr::rename(event = "pathway") |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), .data$path_id) |>
    dplyr::mutate(layer = dplyr::row_number()) |>
    dplyr::ungroup()

  dat <- lapply(1:maxLayer, function(i) {
    layerDat <- dat |>
      dplyr::filter(.data$layer == i) |>
      dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), !!!rlang::parse_exprs(layerColumns[1:i])) |>
      dplyr::reframe(
        layer = .data$layer,
        event = .data$event,
        freq = sum(.data$freq),
        frac = sum(.data$frac),
        xmin = min(.data$xmin),
        xmax = max(.data$xmax)
      ) |>
      dplyr::distinct()
  }) |>
    dplyr::bind_rows()
}

splitCombinations <- function(treatmentPathways, strataX, strataY) {
  layerCols <- names(treatmentPathways)[grepl(pattern = "^layer_\\d$", names(treatmentPathways))]

  n <- sum(grepl(names(treatmentPathways), pattern = "^layer_\\d$"))

  treatmentPathways |>
    dplyr::mutate(
      event_to_split = .data$event
    ) |>
    tidyr::separate_longer_delim(cols = "event_to_split", delim = "+") |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), !!!rlang::parse_exprs(layerCols)) |>
    dplyr::mutate(
      comb_id = dplyr::row_number(),
      comb_max = max(dplyr::row_number())
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ymin = 1 / .data$comb_max * (.data$comb_id - 1) + .data$layer,
      ymax = 1 / .data$comb_max * .data$comb_id + .data$layer
    ) |>
    dplyr::rename(
      event_org = "event",
      event = "event_to_split"
    )
}

plotSunburst <- function(treatmentPathways, strataX, strataY) {
  ggDat <- mergeIndividualPathways(treatmentPathways, strataX, strataY) |>
    splitCombinations(strataX, strataY)

  gg <- ggplot2::ggplot(data = ggDat)

  nLayers <- sum(grepl(pattern = "^layer_\\d$", names(ggDat)))

  for (i in 1:nLayers)
    gg <- gg + ggplot2::geom_rect(
      data = ggDat |>
        dplyr::filter(.data$layer == i),
      mapping = ggplot2::aes(
        ymin = .data$ymin,
        ymax = .data$ymax,
        xmin = .data$xmin,
        xmax = .data$xmax,
        fill = .data$event
      ),
      colour = "#000000"
    )

  gg +
    ggplot2::coord_polar() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0, nLayers + 1) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!!rlang::parse_exprs(strataY)),
      cols = ggplot2::vars(!!!rlang::parse_exprs(strataX))
    )
}

ggSunburst <- function(treatmentPathways, minFreq = 0, strataX = "", strataY = "", style = "default") {
  collection <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(x = treatmentPathways, min.cols = 2, add = collection)
  checkmate::assertNames(x = names(treatmentPathways), must.include = c("pathway", "freq"), .var.name = "treatmentPathways", add = collection)
  checkmate::assertIntegerish(x = minFreq, lower = 0, len = 1, add = collection)
  checkmate::assertCharacter(x = strataX, add = collection)
  checkmate::assertCharacter(x = strataY, add = collection)
  checkmate::assertCharacter(x = style, len = 1, add = collection)
  checkmate::reportAssertions(collection)

  colNames <- names(treatmentPathways)

  extraCols <- colNames[!colNames %in% c("pathway", "freq")]

  colGroups <- treatmentPathways |>
    dplyr::group_by(!!!rlang::parse_exprs(extraCols)) |>
    dplyr::reframe()

  cols <- sapply(extraCols, function(col) {
    groups <- unique(colGroups[[col]])
    if (length(groups) > 1) {
      sprintf("%s: %s", col, paste(sprintf("`%s`", groups), collapse = ", "))
    }
  })

  cols <- cols[!sapply(cols, is.null)] |>
    unlist() |>
    as.character()

  if (length(cols) > 0) {
    warning(sprintf("Found columns with multiple groups: %s. You can pass the columns as strata in: `strataX` and/or `strataY` ", cols))
  }

  gg <- treatmentPathways |>
    dplyr::filter(.data$freq >= minFreq) |>
    plotSunburst(strataX, strataY)

  if (minFreq >= 0) {
    nPaths <- treatmentPathways |>
      dplyr::filter(.data$freq < minFreq) |>
      nrow()
    message(sprintf("Filtered out %s pathways with a frequency < %s", nPaths, minFreq))
  }

  if (style == "darwin") {
    gg <- gg +
      ggThemeDarwin(fontSize = 10)
  }

  return(gg)
}

getFreqRanges <- function(treatmentPathways) {
  pathFreqChoices <- list(
    maximum = max(treatmentPathways$freq),
    `99%` = round(quantile(treatmentPathways$freq, probs = 0.99)),
    `97.5%` = round(quantile(treatmentPathways$freq, probs = 0.975)),
    `95%` = round(quantile(treatmentPathways$freq, probs = 0.95)),
    `75%` = round(quantile(treatmentPathways$freq, probs = 0.75)),
    median = round(median(treatmentPathways$freq)),
    mean = round(mean(treatmentPathways$freq)),
    `25%` = round(quantile(treatmentPathways$freq, probs = 0.25)),
    `5%` = round(quantile(treatmentPathways$freq, probs = 0.05)),
    `2.5%` = round(quantile(treatmentPathways$freq, probs = 0.025)),
    `1%` = round(quantile(treatmentPathways$freq, probs = 0.01)),
    minimum = min(treatmentPathways$freq)
  )

  names(pathFreqChoices) <- sprintf("%s (%s)", names(pathFreqChoices), unlist(pathFreqChoices))

  return(pathFreqChoices)
}

plotShinyEventDuration = function(eventDurations, minCellCount = 0, treatmentGroups = "both", eventLines = NULL, includeOverall = TRUE, xLab = "days") {
  eventDurations <- eventDurations |>
    dplyr::filter(
      .data$event_count >= minCellCount,
      dplyr::case_when(
        treatmentGroups == "both" ~ .data$event_name == .data$event_name,
        treatmentGroups == "group" ~ .data$event_name %in% c("mono-event", "combination-event"),
        treatmentGroups == "individual" ~ !.data$event_name %in% c("mono-event", "combination-event")
      ),
      dplyr::case_when(
        is.null(eventLines) ~ .data$event_name == .data$event_name,
        .default = .data$line %in% c(as.character(eventLines), "overall")
      ),
      dplyr::case_when(
        includeOverall ~ .data$event_name == .data$event_name,
        .default = !.data$line == "overall"
      )
    )

  ggplot2::ggplot(data = eventDurations) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(
        group = interaction(.data$event_name, .data$cdm_name, .data$description),
        y = .data$event_name,
        xmin = .data$duration_min,
        xlower = .data$duration_q1,
        xmiddle = .data$duration_median,
        xupper = .data$duration_q2,
        xmax = .data$duration_max
      ),
      stat = "identity"
    ) +
    ggplot2::facet_grid(.data$cdm_name + .data$description ~ .data$line) +
    ggplot2::labs(
      title = "Duration of events per line",
      x = xLab
    ) +
    ggThemeDarwin(fontSize = 10)
}
