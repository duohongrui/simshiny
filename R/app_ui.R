#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinythemes shinyWidgets
#' @importFrom tools file_ext
#' @importFrom stats setNames na.omit runif IQR
#' @importFrom utils write.csv read.table install.packages
#' @importFrom Hmisc capitalize
#' @importFrom splatter setParams
#' @importFrom methods slotNames is
#' @importFrom openxlsx write.xlsx
#' @importFrom assertthat assert_that
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr lst
#' @importFrom umap umap
#' @import ggplot2
#' @noRd
app_ui <- function(request) {

  navbarPage(
    theme = shinythemes::shinytheme("yeti"),
    title = "Simsite",
    tags$head(shiny::includeCSS(system.file("app/www/style.css", package = "simshiny")),
              ### size of column names
              tags$style(HTML("thead {font-size: 14px;}")),
              ),
    ### method choice
    method_choice_ui(),
    ### online simulation
    online_simulation_ui()
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "simshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
