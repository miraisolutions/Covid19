#' lineplots_day_contagion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_lineplots_day_contagion_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(inputId = ns("radio_log_linear"), label = "",
                 choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE),
    plotOutput(ns("line_plot_day_contagion"))
  )
}

#' lineplots_day_contagion Server Function
#'
#' @param countries_data reactive data.frame for multiple countries
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_lineplots_day_contagion_server <- function(input, output, session, countries_data){
  ns <- session$ns

  statuses <- c("confirmed", "deaths", "recovered", "active")
  output$line_plot_day_contagion <- renderPlot({
    df <- countries_data() %>%
      select(-starts_with("new_"), -contagion_day) %>%
      arrange(date) %>%
      pivot_longer(cols = -c(date, Country.Region), names_to = "status", values_to = "value") %>%
      mutate(Country.Region = as.factor(Country.Region)) %>%
      mutate(status = factor(status, levels = statuses))


    df %>%
      time_evol_line_facet_plot(log = input$radio_log_linear)
  })

}


