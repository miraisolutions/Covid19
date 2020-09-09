if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(RColorBrewer)
  library(COVID19)
  library(plotly)
  library(shinycssloaders)
  library(DT)
  library(grid)
  library(scales)

  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_country_ui("country")
  )
  server <- function(input, output) {

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    n = 1000; w = 7

    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)

    countries <- reactive({
      data_filtered %>%
        select(Country.Region) %>%
        distinct()
    })

    callModule(mod_country_server, "country",
               data = data_filtered, countries = countries, n = n,  w = w)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

