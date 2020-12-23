if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  #devtools::load_all()
  #sapply(file.path("R",list.files("R")), source)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_stackedbarplot_ui("plot", 5)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    callModule(mod_stackedbarplot_status_server, "plot", orig_data_aggregate, active_hosp = TRUE, istop = TRUE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

