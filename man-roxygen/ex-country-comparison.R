if (interactive()) {


  #pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_country_comparison_ui("country_comparison")
  )
  server <- function(input, output) {

    # Data ----
    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    orig_data_aggregate = orig_data_aggregate %>%
      filter(!is.na(continent))

    n = 1000; w = 7

    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)

    countries <- reactive({
      data_filtered %>%
        select(Country.Region) %>%
        distinct()
    })

    callModule(mod_country_comparison_server, "country_comparison",
               data = orig_data_aggregate, countries = countries, nn = n, n.select = n)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

