if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_stackedbarplot_ui("plot", 5)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    callModule(Covid19Mirai:::mod_stackedbarplot_status_server, "plot", orig_data_aggregate)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

