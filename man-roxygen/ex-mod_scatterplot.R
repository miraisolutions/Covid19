if (interactive()) {
  library(shiny)
  library(Covid19)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_scatterplot_ui("plot", 5)
    )
  )
  server <- function(input, output, session) {

    orig_data_aggregate_today = reactive({ get_timeseries_full_data() %>%
      get_timeseries_by_contagion_day_data() %>%
      aggregate_province_timeseries_data() %>%
      add_growth_death_rate() %>%
      arrange(Country.Region) %>%
      filter( date == max(date))})

    world1000 = reactive({orig_data_aggregate_today() %>% align_country_names_pop() %>%
      mutate(country_name = Country.Region) %>%
      get_pop_data() %>%
      mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
             prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3)) %>%
      select(-country_name) %>%
      align_country_names_pop_reverse() %>%
      filter(confirmed > 10000)})# select those with more than 10000
    # compute stats for all growth factors
    med_growth = reactive({apply(world1000()[, grepl("growth", names(world1000())), drop = F],2,  median)})
    med_prevalence = reactive({median(world1000()$prevalence_rate_1M_pop)})

    world5 = reactive({world1000() %>%
      arrange(desc(confirmed) ) %>%
      head(5) %>%
      select(Country.Region,date,starts_with("growth"),prevalence_rate_1M_pop)})

    callModule(Covid19:::mod_scatterplot_server, "plot", world5(), 5, med = list("x" = med_prevalence(), "y" = med_growth()))

    #callModule(Covid19:::mod_growth_death_rate_server, "plot", orig_data_aggregate)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

