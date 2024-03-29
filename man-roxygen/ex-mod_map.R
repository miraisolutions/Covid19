if (interactive()) {

  #devtools::load_all()

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_map_ui("map_ui")
    )
  )


  server <- function(input, output, session) {

    #countries_data_map <- load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))

    TOTAL <- DATA$TOTAL

    orig_data_aggregate_today <- TOTAL$orig_data_aggregate_today

    # orig_data_aggregate <- DATA$orig_data_aggregate

    countries_data_map = DATA$countries_data_map

    # # map ----
    # data7_orig_data_aggregate = lw_vars_calc(orig_data_aggregate)
    # data14_orig_data_aggregate = lw_vars_calc(orig_data_aggregate, 14)
    #
    #
    # # create datasets for maps merging today with data7
    # orig_data_aggregate_maps = orig_data_aggregate %>%
    #   right_join(data7_orig_data_aggregate %>% select(-population)) %>%
    #   right_join(data14_orig_data_aggregate %>% select(-population))

    callModule(mod_map_server, "map_ui", orig_data_aggregate_today, countries_data_map)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

