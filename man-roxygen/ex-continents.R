if (interactive()) {
  library(shiny)
  library(dplyr)
  library(Covid19)
  library(tidyr)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel("Summary",
                        id = "tab_global",
    Covid19:::mod_continent_comparison_ui("continent_comparison")
               )))
  )
  server <- function(input, output) {

    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })
    pop_data = get_pop_data()

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        align_country_names_pop_reverse() %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
    n = 1000; w = 7
    data_filtered <- reactive({
      orig_data_aggregate() %>%
        Covid19:::rescale_df_contagion(n = n, w = w)
    })

    # countries <- reactive({
    #   data_filtered() %>%
    #     select(Country.Region) %>%
    #     distinct()
    # })
    callModule(Covid19:::mod_continent_comparison_server, "continent_comparison", orig_data_aggregate = orig_data_aggregate, data_filtered = data_filtered, n = n, w = w, pop_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

