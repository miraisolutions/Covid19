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
  library(leaflet)
  library(leaflet.extras)

  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_ind_country_ui("ind_country")
  )
  server <- function(input, output) {

    country = "Switzerland"

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    orig_data_aggregate = orig_data_aggregate %>% filter(Country.Region == country)
    n = 100; w = 7

    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)

    countries <- reactive({
      data_filtered %>%
        select(Country.Region) %>%
        distinct()
    })

    callModule(mod_ind_country_server, "ind_country",
               data = data_filtered, country = country, n = n,  w = w)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

