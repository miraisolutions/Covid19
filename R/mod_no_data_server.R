#' No data in variable text Server Function
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param country character country name
#' @param what variable name
#'
mod_nodata_areaplot_server <- function(input, output, session, country, what = "Vaccines") {

  ns <- session$ns

  # output[[uiselect]] <- renderUI({
  #   selectInput(label = div(style = "font-size:10px","Area"), inputId = ns("select_area"), choices = sort(countries()$Country.Region), selected = sort(countries()$Country.Region)[1])
  # })
  # observeEvent(input$select_area,  {    #
  #   #if (input$select_area == "" || (!(input$select_area %in% df$Country.Region))) {
  #     invisible()
  #   #}
  # })

  output[[uielement]] = renderUI({
    div( class = "count-box",
         style = "color: white; max-width: 100%; background-color: #3c8dbc; margin-left: 20px; margin-right: 20px; font-style: italic; white-space: nowrap; word-wrap: break-word",
         HTML(
           paste0("  No ",what," data for ",strong(country), ".")
         ), align = "left")
  })
}
