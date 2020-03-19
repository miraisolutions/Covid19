#' plot_log_linear UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_plot_log_linear_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(inputId = ns("radio_log_linear"), label = "",
                 choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE),
    plotlyOutput(ns("plot_log_linear"), height = 400)
  )
}

#' plot_log_linear Server Function
#'
#' @param df reactive data.frame
#' @param type character string. Either area or line. Used to select plot type.
#'
#' @example man-roxygen/ex-plot_log_linear.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#'
#' @noRd
mod_plot_log_linear_server <- function(input, output, session, df, type){
  ns <- session$ns

  observeEvent(input$radio_log_linear, {
    output$plot_log_linear <- renderPlotly({

      if (type == "area") {
        p <- df() %>% time_evol_area_plot() %>% fix_colors()
      } else {
        p <- df() %>% time_evol_line_plot()
      }

      if (input$radio_log_linear == "linear") {
      p <- p %>% ggplotly()
      } else {
        p <- p %>% add_log_scale() %>% ggplotly()
      }

      p
    })
  })


}
