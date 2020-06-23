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
#' @param g_palette character vector of colors for the graph and legend
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_lineplots_day_contagion_server <- function(input, output, session, countries_data, g_palette = graph_palette){
  ns <- session$ns
  # countries_ordered <- reactive({
  #   countries_data() %>%
  #     group_by(Country.Region) %>% #TODO: group_by before filter
  #     filter(date == max(date)) %>%
  #     filter(confirmed ==  max(confirmed)) %>%
  #     ungroup() %>%
  #     #arrange(desc(confirmed)) %>%
  #     select(Country.Region) %>%
  #     pull()
  # })

  statuses <- c("confirmed", "deaths", "recovered", "active")
  output$line_plot_day_contagion <- renderPlot({
    df <- countries_data() %>%
      select(statuses, date, Country.Region) %>%
      arrange(date) %>%
      pivot_longer(cols = -c(date, Country.Region), names_to = "status", values_to = "value") %>%
      #mutate(Country.Region = factor(Country.Region, levels = countries_ordered())) %>%
      mutate(status = factor(status, levels = statuses))

    df %>%
      time_evol_line_facet_plot(log = input$radio_log_linear, g_palette = g_palette)
  })

}


