if (interactive()) {
  library(shiny)
  library(Covid19)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_stackedbarplot_ui("plot", 5)
    )
  )
  server <- function(input, output, session) {

    orig_data_aggregate = reactive({get_timeseries_full_data() %>%
      get_timeseries_by_contagion_day_data() %>%
      aggregate_province_timeseries_data() %>%
      arrange(Country.Region)})

    callModule(Covid19:::mod_stackedbarplot_status_server, "plot", orig_data_aggregate)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

