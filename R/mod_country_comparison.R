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
#' @param orig_data reactive data.frame
#'
#' @noRd
mod_country_comparison_server <- function(input, output, session, orig_data){
  ns <- session$ns

}
