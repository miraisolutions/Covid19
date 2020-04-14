#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import markdown
#'
#' @noRd
app_server <- function(input, output, session) {

  orig_data <- reactive({
    get_timeseries_full_data() %>%
      get_timeseries_by_contagion_day_data()
  })

  output$last_update <- renderText({
    paste0("Last updated: ",
           max(orig_data()$date)
           )
  })

  # List the first level callModules here
  callModule(mod_global_server, "global", orig_data = orig_data)
  callModule(mod_country_server, "country", orig_data = orig_data)
  callModule(mod_country_comparison_server, "country_comparison", orig_data = orig_data)

  # what is new pop-up
  observeEvent(input$btn_whatsnew, {
    showModal(modalDialog(
      title = "What's New:",
      includeMarkdown("./NEWS.md"),
      footer = modalButton("Dismiss"),
      size = "l",
      easyClose = TRUE,
      fade = FALSE
    ))
  })

}
