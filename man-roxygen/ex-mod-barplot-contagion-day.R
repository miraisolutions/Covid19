if (interactive()) {
  #devtools::load_all()


  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_bar_plot_day_contagion_ui("bar_plot_day_contagion")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    orig_data_with_ch <- get_datahub_fix_ch()
    orig_data       = orig_data_with_ch$orig_data
    orig_data_ch_2  = orig_data_with_ch$orig_data_ch_2

    orig_data = orig_data %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    # data_filtered <-
    #   orig_data_aggregate %>%
    #     Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <-
      orig_data_aggregate %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))


    callModule(mod_bar_plot_day_contagion_server,"bar_plot_day_contagion", country_data = country_data, nn = n, statuses = c("confirmed", "deaths", "vaccines", "active"))
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

