if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_compare_top_countries_plot_ui("plot_compare_100th")
    )
  )
  server <- function(input, output, session) {
    orig_data <- reactive({
      orig_data <- get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data() %>%
        select(-ends_with("rate"))
    })
    callModule(Covid19:::mod_compare_top_countries_plot_server, "plot_compare_100th", orig_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
