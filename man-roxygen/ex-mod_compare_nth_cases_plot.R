if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_compare_nth_cases_plot_ui("plot_compare_nth")
    )
  )
  server <- function(input, output, session) {
    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region)
      orig_data_aggregate
    })

    callModule(Covid19:::mod_compare_nth_cases_plot_server, "plot_compare_nth", orig_data_aggregate)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
