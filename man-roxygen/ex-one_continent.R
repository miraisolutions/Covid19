if (interactive()) {
  library(shiny)
  library(dplyr)
  library(Covid19)
  library(tidyr)
  require(DT)
  sapply(file.path("R",list.files("R")), source)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel("Africa",
                        id = "tab_global",
    mod_continent_ui("cont_comparison", "africa")
               )))
  )
  server <- function(input, output) {

    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })
    pop_data = get_pop_data()
    countries_data_map <- load_countries_data(destpath = system.file("./countries_data", package = "Covid19"))

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        align_country_names_pop_reverse() %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
    n = 1000; w = 7
    data_filtered <- reactive({
      orig_data_aggregate() %>%
        rescale_df_contagion(n = n, w = w)
    })

    callModule(mod_continent_server, "cont_comparison", orig_data_aggregate = orig_data_aggregate,
               countries_data_map, n = n, w = w, pop_data, cont = "Africa", uicont = "africa")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

