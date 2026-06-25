test_that("CohortCharacteristics: CohortCodelist", {
  skip_if_not_installed(
    c("CohortCharacteristics", "PatientProfiles", "CDMConnector")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = list(pharyngitis = 4112343L),
    name = "my_cohort"
  )

  result <- CohortCharacteristics::summariseCohortCodelist(cdm$my_cohort)

  mod <- suppressWarnings(moduleCohortCodelist(result, .softValidation = TRUE))

  shiny::testServer(app = mod$server, {
    testthat::expect_true(is.character(session$token))
  })
})
