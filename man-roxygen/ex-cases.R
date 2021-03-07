if (interactive()) {
  devtools::load_all()

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui("count-boxes")
  )
  server <- function(input, output) {

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)
    total <-
      orig_data_aggregate %>%
      get_timeseries_global_data() %>% mutate(Country.Region = "World") %>%
      get_timeseries_by_contagion_day_data()

    total_aggregate <- total %>%  # add additional vars
      build_data_aggr()

    total_today <-
      total %>%
      filter(date == max(date))


    callModule(mod_caseBoxes_server, "count-boxes", total_today)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


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
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui("count-boxes")
  )
  server <- function(input, output) {

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)
    total <-
      orig_data_aggregate %>%
      get_timeseries_global_data() %>% mutate(Country.Region = "World") %>%
      get_timeseries_by_contagion_day_data()

    total_aggregate <- total %>%  # add additional vars
      build_data_aggr()

    total_today <-
      total %>%
      filter(date == max(date))


    callModule(mod_caseBoxes_server, "count-boxes", total_today, vax = "recovered")

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

