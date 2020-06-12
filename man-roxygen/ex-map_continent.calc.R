if (interactive()) {
  library(shiny)
  library(dplyr)
  library(Covid19)
  library(tidyr)
  library(plotly)
  library(leaflet)
  library(shinycssloaders)

  cont = "LatAm & Carib."
  cont = "Asia"

  variable = "growth vs prevalence" # set variable
  #variable = "death rate" # set variable
  variable = "prevalence rate" # set variable
  #variable = "active" # set variable
 #variable = "growth factor" # set variable

 #sapply(file.path("R",list.files("R")), source)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_map_cont_calc_ui("map_cont_calc_ui")
    )
  )
  server <- function(input, output) {

    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })
    pop_data = get_pop_data()
    countries_data_map <- Covid19:::load_countries_data_map(destpath = system.file("./countries_data", package = "Covid19"))

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        align_country_names_pop_reverse() %>%
        filter(continent == cont) %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
    # data_filtered <- reactive({
    #   orig_data_aggregate() %>%
    #     Covid19:::rescale_df_contagion(n = n, w = w)
    # })



    callModule(mod_map_cont_cal_server, "map_cont_calc_ui", orig_data_aggregate = orig_data_aggregate,  countries_data_map,
               cont = cont, variable = variable)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

