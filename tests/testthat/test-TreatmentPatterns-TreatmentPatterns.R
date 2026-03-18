test_that("Creation", {
  skip_if_not(require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  tp <- data.frame(
    pathway = c("A+B-C", "B+C-A", "B-A+C"),
    freq = 100,
    sex = "all",
    age = "all",
    index_year = "all",
    target_cohort_name = "test"
  )

  cdm_source <- structure(list(cdm_source_name = "Synthea synthetic health database",
                 cdm_source_abbreviation = "Synthea", cdm_holder = "OHDSI Community",
                 source_description = "SyntheaTM is a Synthetic Patient Population Simulator. The goal is to output synthetic, realistic (but not real), patient data and associated health records in a variety of formats.",
                 source_documentation_reference = "https://synthetichealth.github.io/synthea/",
                 cdm_etl_reference = "https://github.com/OHDSI/ETL-Synthea",
                 source_release_date = "2019-05-25", cdm_release_date = "2019-05-25",
                 cdm_version = "v5.3.1", vocabulary_version = "v5.0 18-JAN-19",
                 analysis_id = 1L), class = "data.frame", row.names = c(NA,
                                                                        -1L))

  tpMod <- TreatmentPathways$new(treatmentPathways = tp,
                                 cdmSourceInfo = cdm_source)

  expect_identical(class(tpMod), c("TreatmentPathways", "ShinyModule", "R6"))

  # tpMod$sunburstCols <- list(
  #   domain = list("A", "B", "C", "A+B", "B+C", "A+C"),
  #   range = list("#ff0000", "#aa0000", "#770000", "#0000ff", "#0000aa", "#000077"))
  #
  # tpMod$sankeyCols <- list(
  #   A = "#ff0000",
  #   B = "#aa0000",
  #   C = "#770000",
  #   `A+B` = "#0000ff",
  #   `B+C` = "#0000a",
  #   `A+C` = "#000077"
  # )
  #
  # expect_true(all(names(tpMod$sankeyCols) %in% c("A", "B", "C", "A+B", "B+C", "A+C")))
  # expect_true(all(tpMod$sunburstCols$domain %in% c("A", "B", "C", "A+B", "B+C", "A+C")))
})
