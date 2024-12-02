ableToRun <- function() {
  all(
    require("TreatmentPatterns", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("shiny", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("shinydashboard", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("dplyr", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )
}

test_that("input to reactive", {
  skip_if_not(ableToRun())

  tp <- read.csv(system.file(
    package = "DarwinShinyModules",
    "dummyData/TreatmentPatterns/csv/treatmentPathways.csv"
  ))
  sunburst <- Sunburst$new(data = tp)
  sankey <- Sankey$new(data = tp)

  sunburst$instanceId <- "A"
  sankey$instanceId <- "B"

  mockApp <- function(id, sunburst, sankey) {
    moduleServer(id, function(input, output, session) {
      sunburst$server(input, output, session)
      sankey$server(input, output, session)

      suppressWarnings(
        session$setInputs(
          Sunburst_A_inputNone = TRUE,
          Sunburst_A_inputGroupCombinations = TRUE,
          Sankey_B_inputNone = TRUE,
          Sankey_B_inputGroupCombinations = TRUE
        )
      )
    })
  }

  testServer(
    app = mockApp,
    args = list(
      sunburst = sunburst,
      sankey = sankey
    ), {
      # Inputs TRUE
      # sunburst
      expect_identical(
        TRUE,
        input$Sunburst_A_inputNone,
        sunburst$inputs$none
      )

      expect_identical(
        TRUE,
        input$Sunburst_A_inputGroupCombinations,
        sunburst$inputs$groupCombinations
      )

      # sankey
      expect_identical(
        TRUE,
        input$Sankey_B_inputNone,
        sankey$inputs$none
      )

      expect_identical(
        TRUE,
        input$Sankey_B_inputGroupCombinations,
        sankey$inputs$groupCombinations
      )

      # Inputs FALSE
      suppressWarnings(
        session$setInputs(
          Sunburst_A_inputNone = FALSE,
          Sunburst_A_inputGroupCombinations = FALSE,
          Sankey_B_inputNone = FALSE,
          Sankey_B_inputGroupCombinations = FALSE
        )
      )

      # sunburst
      expect_identical(
        FALSE,
        input$Sunburst_A_inputNone,
        sunburst$inputs$none
      )

      expect_identical(
        FALSE,
        input$Sunburst_A_inputGroupCombinations,
        sunburst$inputs$groupCombinations
      )

      # sankey
      expect_identical(
        FALSE,
        input$Sankey_B_inputNone,
        sankey$inputs$none
      )

      expect_identical(
        FALSE,
        input$Sankey_B_inputGroupCombinations,
        sankey$inputs$groupCombinations
      )
    }
  )
})
