setCss <- function() {
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
  )
}

bslibTheme <- function() {
  bslib::bs_theme(
    version = 5,
    "navbar-bg" = "#003194"
  )
}

setResources <- function() {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("www", package = "DarwinShinyModules")
  )
}

#' darwinBslib
#'
#' @param appStructure (`list(list())`) Nested list of modules.
#' @param title (`character(1)`) Title of the app.
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#' library(bslib)
#'
#' base <- Text$new("**base**")
#' nested_a <- Text$new("**nested A**")
#' nested_b <- Text$new("**nested B**")
#' sub_a <- Text$new("**sub A**")
#' sub_b <- Text$new("**sub B**")
#' comb_a <- Text$new("**comb A**")
#' comb_b <- Text$new("**comb B**")
#' comb_c <- Text$new("**comb C**")
#'
#' if (interactive()) {
#'   appStructure <- list(
#'     base = base,
#'     nested = list(nested_a, nested_b),
#'     nested_sub = list(
#'       sub_a = sub_a,
#'       sub_b = sub_b
#'     ),
#'     nested_combined = list(
#'       comb_a_b = list(comb_a, comb_b),
#'       comb_c = comb_c
#'     )
#'   )
#'
#'   darwinBslib(appStructure)
#' }
darwinBslib <- function(appStructure, title = NULL) {
  setResources()

  items <- lapply(seq_len(length(appStructure)), function(i) {
    menuName <- names(appStructure[i])
    menu <- appStructure[[i]]
    if ("ShinyModule" %in% class(menu)) {
      bslib::nav_panel(
        title = menuName,
        menu$UI()
      )
    } else if (is.null(names(menu))) {
      bslib::nav_panel(
        title = menuName,
        lapply(menu, function(module) {
          module$UI()
        })
      )
    } else {
      panelItems <- lapply(seq_len(length(menu)), function(j) {
        panelName <- names(menu[j])
        panel <- menu[[j]]
        bslib::nav_panel(
          title = panelName,
          if (all(class(panel) == "list")) {
            lapply(panel, function(module) {
              module$UI()
            })
          } else {
            panel$UI()
          }
        )
      })
      do.call(bslib::nav_menu, args = c(title = menuName, panelItems))
    }
  })

  ui <- bslib::page(
    theme = bslibTheme(),
    setCss(),
    DarwinShinyModules:::darwinHeader(),
    do.call(bslib::navset_bar, items)
  )

  server <- function(input, output, ession) {
    lapply(unlist(appStructure), function(module) {
      module$server(input, output, session)
    })
  }

  shiny::shinyApp(ui, server)
}
