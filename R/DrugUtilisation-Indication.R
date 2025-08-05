Indication <- R6::R6Class(
  classname = "Indication",
  inherit = DarwinShinyModules::ShinyModule,

  # Active ----
  active = list(
    data = function() {
      return(private$.data)
    }
  ),

  # Public ----
  public = list(
    initialize = function(filePath) {
      super$initialize()
      private$.data <- omopgenerics::importSummarisedResult(
        path = file.path(filePath)
      )

      rawDat <- omopgenerics::tidy(private$.data) %>%
        dplyr::mutate(
          cdm_name = factor(.data$cdm_name, levels = unique(.data$cdm_name)),
          cohort_name = factor(.data$cohort_name, levels = unique(.data$cohort_name)),
          age_group = factor(.data$age_group, levels = unique(.data$age_group)),
          pregnancy_period = factor(.data$pregnancy_period, levels = unique(.data$pregnancy_period)),
          variable_name = factor(.data$variable_name, levels = unique(.data$variable_name)),
          variable_level = factor(.data$variable_level, levels = unique(.data$variable_level)),
          window_name = factor(.data$window_name, levels = unique(.data$window_name))
        )

      private$.view_2Mod <- Table$new(data = rawDat, title = NULL, filter = "top")
      private$.view_2Mod$parentNamespace <- self$namespace
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .data = NULL,
    .view_2Mod = NULL,

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "View 1",
            gt::gt_output(outputId = shiny::NS(self$namespace, "view_1"))
          ),
          shiny::tabPanel(
            title = "View 2",
            private$.view_2Mod$UI()
          )
        )
      )
    },
    .server = function(input, output, session) {
      private$.view_2Mod$server(input, outupt, session)
      output$view_1 <- gt::render_gt({
        DrugUtilisation::tableIndication(private$.data, header = "indication")
      })
    }
  )
)
