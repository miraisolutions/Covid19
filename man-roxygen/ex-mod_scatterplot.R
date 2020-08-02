if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_scatterplot_ui("plot")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    inputcountries = reactive(c("Australia","New Zealand")) # example with countries

    callModule(Covid19Mirai:::mod_scatterplot_server, "plot", orig_data_aggregate, istop = F, countries = inputcountries)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_scatterplot_ui("plot_oceania")
    )
  )
  server <- function(input, output, session) {

    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })

    pop_data = get_pop_datahub()
    orig_data_aggregate = reactive({ build_data_aggr(orig_data(), pop_data)})

    cont = "Oceania"

    subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) %>%
      group_by(subcontinent) %>%
      summarize(population = sum(population, na.rm = T))

    orig_data_aggregate_cont <- reactive({
      orig_data_aggregate %>% filter(continent == cont)})
    # select all variables
    allstatuses = get_aggrvars()
    subcontinent_data <- reactive({aggr_to_cont(orig_data_aggregate_cont(), "subcontinent", "date",
                                                subcontinent_pop_data, allstatuses)})
    subcontinents = reactive({unique(orig_data_aggregate_cont()$subcontinent)})


    callModule(Covid19Mirai:::mod_scatterplot_server, "plot_oceania", subcontinent_data, istop = F, countries = subcontinents)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


