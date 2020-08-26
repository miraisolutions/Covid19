if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(RColorBrewer)
  library(DT)
  library(leaflet)
  library(leaflet.extras)
  library(scales)
  library(grid)
  library(shinycssloaders)
  library(plotly)
  library(COVID19)

  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel("Summary",
                        id = "tab_global",
    mod_continent_comparison_ui("continent_comparison")
               )))
  )
  server <- function(input, output) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    n = 1000; w = 7
    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    # countries <- reactive({
    #   data_filtered() %>%
    #     select(Country.Region) %>%
    #     distinct()
    # })
    callModule(mod_continent_comparison_server, "continent_comparison", orig_data_aggregate = orig_data_aggregate, n = n, w = w, pop_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

