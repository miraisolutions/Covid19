#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      h1("Covid19"),
      tabsetPanel(
        id = "main_ui",
        tabPanel("Global",
                 id = "tab_global",
                 mod_global_ui("global")),
        tabPanel("Country",
                 id = "tab_global",
                 mod_country_ui("country")),
        tabPanel("Countries Comparison",
                 id = "tab_global",
                 mod_country_comparison_ui("country_comparison"))
      )
    )
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Covid19'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

