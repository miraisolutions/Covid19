#' global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_global_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' global Server Function
#'
#' @noRd 
mod_global_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_global_ui("global_ui_1")
    
## To be copied in the server
# callModule(mod_global_server, "global_ui_1")
 
