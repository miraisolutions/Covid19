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
    splitLayout(
      cellWidths = c("50%", "50%"),
      radioButtons(inputId = ns("radio_log_linear"), label = "",
                   choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE),
      radioButtons(inputId = ns("radio_1Mpop"), label = "",
                 choices = c("Total" = "tot", "Over 1M people" = "oneMpop"), selected = "oneMpop", inline = TRUE)
    ),
    plotOutput(ns("line_plot_day_contagion"))
  )
}

#' lineplots_day_contagion Server Function
#'
#' @param countries_data data.frame for multiple countries
#' @param g_palette character vector of colors for the graph and legend
#' @param nn minimum date derived from first day with more than nn cases
#' @param statuses variables to be used in barplot, 4  of .case.colors, default c("confirmed", "deaths", "recovered", "active")
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_lineplots_day_contagion_server <- function(input, output, session, countries_data, g_palette = graph_palette, nn, statuses = c("confirmed", "deaths", "recovered", "active")){
  ns <- session$ns

  mindate = min(countries_data$date[countries_data$confirmed>nn], na.rm = TRUE)
  countries_data = countries_data %>% filter(date >= mindate)

  data = reactive({
    if (input$radio_1Mpop == "oneMpop"){
      countries_data %>%   mutate( # add aggregated vars
        across(all_of(as.vector(statuses)), ~oneM_pop_calc(.x,pop = population)) # use all_of
      )
    } else
      countries_data
  })

  output$line_plot_day_contagion <- renderPlot({
    df <- data() %>%
      select(statuses, date, Country.Region) %>%
      arrange(date) %>%
      pivot_longer(cols = -c(date, Country.Region), names_to = "status", values_to = "value") %>%
      #mutate(Country.Region = factor(Country.Region, levels = countries_ordered())) %>%
      mutate(status = factor(status, levels = statuses))

    df %>%
      time_evol_line_facet_plot(log = input$radio_log_linear, g_palette = g_palette)
  })

}


