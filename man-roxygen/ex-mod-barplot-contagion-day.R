if (interactive()) {
  library(shiny)
  library(Covid19)
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19:::mod_bar_plot_day_contagion_ui("bar_plot_day_contagion")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })
    pop_data = get_pop_data()

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

    data_filtered <- reactive({
      orig_data_aggregate() %>%
        Covid19:::rescale_df_contagion(n = n, w = w)
    })

    country_data <- reactive({
      data_filtered() %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })



    callModule(Covid19:::mod_bar_plot_day_contagion_server,"bar_plot_day_contagion", country_data = country_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

