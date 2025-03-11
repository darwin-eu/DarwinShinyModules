BslibApp <- R6::R6Class(
  classname = "BslibApp",
  inherit = App,

  ## Public ----
  public = list(
    ## Methods ----
    initialize = function(appStructure, title = NULL) {
      super$initialize(appStructure)
      private$.title <- title
    },

    UI = function() {
      bslib::page(
        do.call(bslib::navset_bar, private$parseModules())
      )
    }
  ),

  ## Private ----
  private = list(
    ### Fields ----
    .title = NULL,

    ### Methods ----
    parseModules = function() {
      lapply(seq_len(length(private$.appStructure)), function(i) {
        menuName <- names(private$.appStructure[i])
        menu <- private$.appStructure[[i]]
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
    }
  )
)
