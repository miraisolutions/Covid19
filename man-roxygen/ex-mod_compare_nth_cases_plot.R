if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(shinycssloaders)
  library(plotly)
  library(ggplot2)
  library(RColorBrewer)
  library(COVID19)

  library(scales)
  sapply(file.path("R",list.files("R")), source)
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("plot_compare_nth")
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- get_datahub() %>%
        get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    n = 1000; w = 7
    data_filtered <- #reactive({
      orig_data_aggregate %>%
        rescale_df_contagion(n = n, w = w)
   # })
    callModule(mod_compare_nth_cases_plot_server, "plot_compare_nth", data_filtered)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(ggplot2)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("lines_points_plots")
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    n = 1000; w = 7

    countries = c("Switzerland", "Italy", "France")

    countries_data <- #reactive({
      countries_data <- orig_data_aggregate %>%
        filter(Country.Region %in% countries)
    #})
    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               n = n, n_highligth = length(countries), istop = F)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(ggplot2)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("lines_points_plots")
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- get_datahub() %>%
        get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    n = 1000
    countries = c("Switzerland")

    countries_data <-
      countries_data <- orig_data_aggregate %>%
        filter(Country.Region %in% countries)

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               n = n, n_highligth = length(countries), istop = F)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
