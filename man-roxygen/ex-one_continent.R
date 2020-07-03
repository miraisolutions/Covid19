if (interactive()) {
  library(shiny)
  library(dplyr)
  library(Covid19Mirai)
  library(tidyr)
  library(scales)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(grid)
  library(shinycssloaders)
  library(RColorBrewer)
  require(DT)
  sapply(file.path("R",list.files("R")), source)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  cont = "Northern America"
  uicont = "northernamerica"
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel(cont,
                        id = "tab_global",
    Covid19Mirai:::mod_continent_ui("cont_comparison", uicont)
               )))
  )
  server <- function(input, output) {

    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })

    #pop_data = get_pop_data()
    pop_data = get_pop_datahub()
    orig_data_aggregate = reactive({ build_data_aggr(orig_data(), pop_data)})

    countries_data_map <- Covid19Mirai:::load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))

    orig_data_aggregate_cont <- reactive({
      orig_data_aggregate() %>% filter(continent == cont)
    })

    n = 1000; w = 7
    data_filtered <- reactive({
      orig_data_aggregate_cont() %>%
        rescale_df_contagion(n = n, w = w)
    })

    callModule(mod_continent_server, "cont_comparison", orig_data_aggregate = orig_data_aggregate_cont,
               countries_data_map, n = n, w = w, pop_data, cont = cont, uicont = uicont)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

