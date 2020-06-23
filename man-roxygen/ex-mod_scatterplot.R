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

    orig_data <-reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()})

    pop_data = get_pop_data()

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        #align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        #align_country_names_pop_reverse() %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
    inputcountries = reactive(c("Australia","New Zealand")) # example with countries

    callModule(Covid19:::mod_scatterplot_server, "plot", orig_data_aggregate, istop = F, countries = inputcountries)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

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
      Covid19:::mod_scatterplot_ui("plot_oceania", 5)
    )
  )
  server <- function(input, output, session) {

    orig_data <-reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()})

    pop_data = get_pop_data()

    orig_data_aggregate <- reactive({
      orig_data_aggregate <- orig_data() %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        #align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        #align_country_names_pop_reverse() %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
    cont = "Oceania"


    subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) %>%
      group_by(subcontinent) %>%
      summarize(population = sum(population, na.rm = T))

    orig_data_aggregate_cont <- reactive({
      orig_data_aggregate() %>% filter(continent == cont)})
    statuses <- c("confirmed", "deaths", "recovered", "active")
    # select all variables
    allstatuses = c(statuses, paste0("new_", statuses))
    subcontinent_data <- reactive({aggr_to_cont(orig_data_aggregate_cont(), "subcontinent", "date",
                                                subcontinent_pop_data, allstatuses)})
    subcontinents = reactive({unique(orig_data_aggregate_cont()$subcontinent)})


    callModule(Covid19:::mod_scatterplot_server, "plot_oceania", subcontinent_data, istop = F, countries = subcontinents)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


