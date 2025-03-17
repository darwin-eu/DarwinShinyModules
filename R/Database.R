Database <- R6::R6Class(
  classname = "Database",
  inherit = ShinyModule,

  ## Active ----
  active = list(
    connectionDetails = function() {
      return(private$.connectionDetails)
    },

    connected = function() {
      return(!is.null(private$.connection))
    }
  ),

  ## Public ----
  public = list(
    ## Methods ----
    query = function() {},
    execute = function() {}
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .connection = NULL,

    ### Methods ----
    finalize = function() {
      private$disconnect()
    },

    .UI = function() {
      shiny::fluidRow(
        shiny::div(
          style = "max-width:5%; float:left; margin-right:5px; margin-top:7px",
          shiny::conditionalPanel(
            condition = "output.connected",
            shiny::icon(
              name = "database",
              lib = "font-awesome",
              style = "color: #59bf3d"
            ),
            ns = shiny::NS(self$namespace)
          ),
          shiny::conditionalPanel(
            condition = "!output.connected",
            shiny::icon(
              name = "exclamation",
              lib = "font-awesome",
              style = "color: #de164f"
            ),
            ns = shiny::NS(self$namespace)
          )
        ),
        div(
          style = "max-width:250px; float:left; margin-right:5px; margin-top:7px",
          shiny::textOutput(outputId = shiny::NS(self$namespace, "databaseName"))
        )
      )
    },

    .server = function(input, output, session) {
      initConnect <- FALSE
      if (!initConnect & !self$connected) {
        private$connect()
        initConnect <- TRUE
      }

      shiny::observeEvent(private$.reactiveValues$connected, {
        if (!private$.reactiveValues$connected) {
          shiny::showModal(
            shiny::modalDialog(
              shiny::div(
                style = "text-align: center;",
                "Disconnected from database!",
                shiny::br(),
                shiny::actionButton(
                  label = "Reconnect",
                  inputId = shiny::NS(self$namespace, "reconnect")
                )
              ),
              title = "Disconnected",
              footer = NULL,
              easyClose = FALSE
            )
          )
        }
      })

      if (self$connected) {
        shiny::onStop(private$disconnect)

        output$connected <- reactive({
          private$.reactiveValues$connected
        })
        shiny::outputOptions(output, "connected", suspendWhenHidden = FALSE)

        output$databaseName <- shiny::renderText({
          private$.reactiveValues$databaseName
        })

        shiny::observeEvent(input$reconnect, {
          private$connect()
          shiny::removeModal()
        })

        shiny::observeEvent(input$disconnect, {
          private$disconnect()
        })
      }
    },

    connect = function() {},
    disconnect = function() {}
  )
)
