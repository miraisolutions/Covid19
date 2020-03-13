#' country_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' country_comparison Server Function
#'
#' @noRd 
mod_country_comparison_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_country_comparison_ui("country_comparison_ui_1")
    
## To be copied in the server
# callModule(mod_country_comparison_server, "country_comparison_ui_1")
 
