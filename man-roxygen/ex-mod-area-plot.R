if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("plot_log_area_global")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    data_filtered <-
      orig_data_aggregate() %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <- reactive({
      data_filtered() %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })
    levs <- Covid19Mirai:::sort_type_hardcoded()

    df_tot <- reactive({
      country_data() %>%
        #select(-Country.Region, -contagion_day) %>%
        select(date, !!levs) %>%
        #select(-starts_with("new"), -confirmed, -starts_with("growth_"), -ends_with("_rate"), -contains("1M")) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = factor(status, levels = levs)) %>%
        capitalize_names_df()
    })


    callModule(mod_plot_log_linear_server,"plot_log_area_global", df = df_tot, type = "line")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

