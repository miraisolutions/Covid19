#' country UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_country_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' country Server Function
#'
#' @noRd 
mod_country_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_country_ui("country_ui_1")
    
## To be copied in the server
# callModule(mod_country_server, "country_ui_1")
 
