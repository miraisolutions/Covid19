if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_growth_death_rate_ui("plot")
    )
  )
  server <- function(input, output, session) {

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

    callModule(Covid19:::mod_growth_death_rate_server, "plot", orig_data_aggregate)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

