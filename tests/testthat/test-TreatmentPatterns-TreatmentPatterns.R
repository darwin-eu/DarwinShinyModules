test_that("Creation", {
  skip_if_not(require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  skip_if_not(require("CDMConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))

  if (interactive()) {
    library(TreatmentPatterns)
    library(testthat)
    library(DarwinShinyModules)
    source("./tests/testthat/helper-TreatmentPatterns.R")
    source("./R/TreatmentPatterns-TreatmentPatterns.R")
    source("./R/utils.R")
  }

  tpr <- makeTreatmentPatternsResult()

  testthat::skip_if(is.null(tpr))

  tpMod <- moduleTreatmentPatterns(tpr)

  expect_identical(class(tpMod), c("TreatmentPatterns", "ShinyModule", "R6"))

  # UI
  expect_s3_class(tpMod$UI(), "shiny.tag.list")

  # Server
  server <- function(input, output, session) {}
  shiny::testServer(app = tpMod$server, {
    expect_true(is.character(session$token))
    session$setInputs(
      # General
      !!rlang::sym(shiny::NS(tpMod$namespace, "cdmName")) := "Synthea",
      !!rlang::sym(shiny::NS(tpMod$namespace, "targetCohort")) := "viralsinusitis",
      !!rlang::sym(shiny::NS(tpMod$namespace, "analysis")) := "",

      # Pathways
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpMinFreq")) := "maximum (211)",
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpMaxFreq")) := "maximum (211)",
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpAge")) := "all",
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpSex")) := "all",
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpIndexYear")) := "all",
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpFacetX")) := NULL,
      !!rlang::sym(shiny::NS(tpMod$namespace, "tpFacetY")) := NULL,

      # Event Duration
      !!rlang::sym(shiny::NS(tpMod$namespace, "treatmentGroups")) := "both",
      !!rlang::sym(shiny::NS(tpMod$namespace, "eventLines")) := NULL,
      !!rlang::sym(shiny::NS(tpMod$namespace, "includeOverall")) := "Yes",
      !!rlang::sym(shiny::NS(tpMod$namespace, "logXAxis")) := "Yes",

      # Counts
      !!rlang::sym(shiny::NS(tpMod$namespace, "countsAge")) := 1:5,
      !!rlang::sym(shiny::NS(tpMod$namespace, "countsSex")) := c("MALE"),
      !!rlang::sym(shiny::NS(tpMod$namespace, "countsYear")) := as.character(1950:1955)
    )
    # Pathways
    testthat::expect_equal(nrow(tpMod$.__enclos_env__$private$.treatmentPathwaysTable$args$result), 1)
    testthat::expect_equal(nrow(tpMod$.__enclos_env__$private$.treatmentPathwaysSunburst$args$treatmentPathways), 1)

    # Event Duration
    testthat::expect_equal(
      tpMod$.__enclos_env__$private$.treatmentDurationPlot$args$eventDurations$duration_min,
      tpMod$.__enclos_env__$private$.treatmentDurationTable$args$result$duration_min
    )

    # Counts
    testthat::expect_equal(nrow(tpMod$.__enclos_env__$private$.countsSexMod$args$result), 1)
    testthat::expect_equal(nrow(tpMod$.__enclos_env__$private$.countsAgeMod$args$result), 5)
    testthat::expect_equal(nrow(tpMod$.__enclos_env__$private$.countsIndexYearMod$args$result), 6)
  })
})
