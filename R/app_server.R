#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_global_server, "global")
  callModule(mod_country_server, "country")
  callModule(mod_country_comparison_server, "country_comparison")
}
