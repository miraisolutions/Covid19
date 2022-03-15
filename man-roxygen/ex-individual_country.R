if (interactive()) {

  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_ind_country_ui("ind_country")
  )
  server <- function(input, output) {

    country = "Switzerland"

    rds_data = "DATA.rds"
    orig_data_with_ch = readRDS(file =  file.path(system.file("./datahub", package = "Covid19Mirai"),rds_data))

    orig_data_aggregate       = orig_data_with_ch$orig_data_aggregate
    orig_data_ch_2  = orig_data_with_ch$orig_data_ch_2

    # pop_data = get_pop_datahub()
    # orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    orig_data_aggregate = orig_data_aggregate %>% filter(Country.Region == country)
    n = 100; w = 7

    # data_filtered <-
    #   orig_data_aggregate %>%
    #     Covid19Mirai:::rescale_df_contagion(n = n, w = w)
    #
    # countries <- reactive({
    #   data_filtered %>%
    #     select(Country.Region) %>%
    #     distinct()
    # })

    callModule(mod_ind_country_server, "ind_country",
               data = orig_data_aggregate, data2 = orig_data_ch_2, country = country, nn = n,  w = w)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

