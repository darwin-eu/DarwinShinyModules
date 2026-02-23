#' @title Database Decorator Class
#'
#' @include ShinyModule.R
#'
#' @description
#' This class is a `decorator` and is not meant to be directly used, but to be
#' inherited by other modules, like `DatabaseDBC` and `DatabaseDBI`.
#'
#' @details
#' The inherited `Database` modules have their own implementation to connect
#' to, and query the database.
#'
#' To add a new database type it is required to inherit from the `Database`
#' class, to override the private `connect()` and `disconnect()` methods, and
#' to extend it with functionality to query the database.
#'
#' `DatabaseDBC` extends this class with the public `query()` and `execute()`
#' methods, while `DatabaseDBI` extends it with the `attachTables()` and
#' `detatchTables()` methods, and the public `tables` field.
#'
#' @export
Database <- R6::R6Class(
  classname = "Database",
  inherit = ShinyModule,

  ## Active ----
  active = list(
    #' @field connected (`logical(1)`) Logical if connected to the database.
    connected = function() {
      return(!is.null(private$.connection))
    }
  ),

  ## Public ----
  public = list(
    #' @description
    #' Method to upload tables to the database.
    #'
    #' @param tableName (`character(1)`) Name of the table
    #' @param data (`data.frame`) data.frame like table to upload
    uploadTable = function(tableName, data) {
      warning(sprintf("Not implemented for class: %s", class(self)[1]))
    },

    #' @description
    #' Method to connect to the database.
    connect = function() {},

    #' @description
    #' Method to disconnect from the database.
    disconnect = function() {}
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .connection = NULL,

    ### Methods ----
    finalize = function() {
      self$disconnect()
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
      self$connect()

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
        shiny::onStop(self$disconnect)

        output$connected <- reactive({
          private$.reactiveValues$connected
        })
        shiny::outputOptions(output, "connected", suspendWhenHidden = FALSE)

        output$databaseName <- shiny::renderText({
          private$.reactiveValues$databaseName
        })

        shiny::observeEvent(input$reconnect, {
          self$connect()
          shiny::removeModal()
        })

        shiny::observeEvent(input$disconnect, {
          self$disconnect()
        })
      }
    }
  )
)
