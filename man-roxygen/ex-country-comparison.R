if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(RColorBrewer)
  library(plotly)
  library(shinycssloaders)
  library(DT)
  library(grid)
  library(scales)

  sapply(file.path("R",list.files("R")), source)
  #pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_country_comparison_ui("country_comparison")
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

    callModule(mod_country_comparison_server, "country_comparison",
               data = orig_data_aggregate, countries = countries, nn = n, n.select = n)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

