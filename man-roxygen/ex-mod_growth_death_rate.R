if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_growth_death_rate_ui("plot")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    callModule(mod_growth_death_rate_server, "plot", orig_data_aggregate)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

