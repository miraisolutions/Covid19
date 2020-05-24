if (interactive()) {
  library(shiny)
  library(Covid19)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(ggrepel)

  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_scatterplot_ui("plot", 5)
    )
  )
  server <- function(input, output, session) {

    orig_data <-reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()})

    orig_data_aggregate <-reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region)})

    inputcountries = reactive(c("UK","Italy", "Slovakia","Greece","Russia", "Brazil")) # example with countries

    callModule(Covid19:::mod_scatterplot_server, "plot", orig_data_aggregate, istop = F, countries = inputcountries)

    #callModule(Covid19:::mod_growth_death_rate_server, "plot", orig_data_aggregate)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

