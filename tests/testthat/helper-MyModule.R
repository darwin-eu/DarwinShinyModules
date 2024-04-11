library(datasets)

MyModule <- R6::R6Class(
  classname = "MyModule",
  inherit = ShinyModule,

  public = list(
    UI = function() {
      shiny::tagList(
        shiny::tableOutput(outputId = shiny::NS(private$.appId, private$id("myTable"))),
        shiny::uiOutput(outputId = shiny::NS(private$.appId, private$id("moduleData")))
      )
    },

    server = function(input, output, session) {
      output[[private$id("myTable")]] <- shiny::renderTable({
        head(private$.data)
      })

      output[[private$id("moduleData")]] <- shiny::renderUI({
        shiny::HTML(paste(
          sprintf("App ID: %s", private$.appId),
          sprintf("Module Name: %s", private$.moduleName),
          sprintf("Instance ID: %s", private$.instanceId),
          sprintf("Namespace ID myTable: %s", private$id("myTable")),
          sprintf("Namespace ID moduleData: %s", private$id("moduleData")),
          sep = "<br/>"
        ))
      })
    }
  ),

  private = list(
    .data = iris
  ),

  active = list(
    data = function() return(private$.data)
  )
)
