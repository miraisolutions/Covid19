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
    selectInput(label = "Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE),

    uiOutput(ns("barplots"))
  )
}

#' country_comparison Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#'
#' @noRd
mod_country_comparison_server <- function(input, output, session, orig_data){
  ns <- session$ns

  countries <- reactive({
    orig_data() %>%
      select(Country.Region) %>%
      distinct()
  })

  observe(
    updateSelectInput(session, "select_countries", choices = countries()$Country.Region, selected = c("Switzerland", "Italy"))
  )

  observeEvent(input$select_countries,{
    # Data ----
    countries_data <- reactive({orig_data() %>%
        aggregate_province_timeseries_data() %>%
        filter(Country.Region %in% input$select_countries) %>%
        filter(contagion_day > 0) %>%
        select(-ends_with("rate")) %>%
        arrange(desc(date))
    })

    # Bar plots ----
    output$barplots <- renderUI({
      for (country in unique(countries_data()$Country.Region)) {
        h2(country)
        mod_bar_plot_day_contagion_ui(paste0(country,"_bar_plot_day_contagion"))
      }
    })

    for (country in unique(countries_data()$Country.Region)) {
      country_data <- countries_data() %>%
        filter(Country.Region %in% input$country)
      callModule(mod_bar_plot_day_contagion_server, paste0(country,"_bar_plot_day_contagion"), country_data)
    }
  })

}
