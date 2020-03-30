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
      orig_data <- get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })

    world <- reactive({
      orig_data() %>%
        aggregate_country_data()
    })

    world_top_5 <- reactive({
      world() %>%
        head(5)
    })

    callModule(Covid19:::mod_growth_death_rate_server, "plot", world_top_5)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
