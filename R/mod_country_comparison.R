#' country_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_country_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(label = "Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE),

    uiOutput(ns("barplots")),
    uiOutput(ns("lineplots")),
    mod_add_table_ui(ns("add_table_countries"))
  )
}

#' country_comparison Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @import dplyr
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
    if (input$select_countries != "") {
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
        lapply(input$select_countries, function(country){
          tagList(
            h2(country),
            mod_bar_plot_day_contagion_ui(ns(paste0(country,"_bar_plot_day_contagion")))
          )
        })
      })

      lapply(input$select_countries, function(country){
        country_data <- reactive({countries_data() %>%
            filter(Country.Region %in% country)})
        callModule(mod_bar_plot_day_contagion_server, paste0(country,"_bar_plot_day_contagion"), country_data)
      })
    }

    # Line plots ----
    output$lineplots <- renderUI({
      tagList(
        h2("Countries Comparison"),
        mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion"))
      )
    })

    callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion", countries_data)

    # tables ----
    callModule(mod_add_table_server, "add_table_countries", countries_data)
  })

}
