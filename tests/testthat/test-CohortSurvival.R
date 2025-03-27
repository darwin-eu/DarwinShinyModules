test_that("CohortSurvival: Server", {
  skip_if_not(
    all(
      require("CohortSurvival", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("CDMConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  )

  cdm <- CohortSurvival::mockMGUS2cdm()

  MGUS_death <- estimateSingleEventSurvival(
    cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "death_cohort",
    strata = list(
      c("age_group"),
      c("sex"),
      c("age_group", "sex")
    )
  )

  cs <- CohortSurvival$new(data = MGUS_death)

  modServer <- function(id) {
    cs$server(input, output, session)
  }

  testServer(modServer, {
    args <- list(
      plotFacet = "age_group",
      plotColour = "sex"
    )

    names(args) <- shiny::NS(cs$inputPanel$moduleId, names(args))
    do.call(what = session$setInputs, args = args)

    expect_equal(isolate(cs$inputPanel$inputValues$plotFacet), isolate(cs$plot$args$facet))
    expect_equal(isolate(cs$inputPanel$inputValues$plotColour), isolate(cs$plot$args$colour))
  })
})
