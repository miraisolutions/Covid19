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
    rds_map = "WorldMap_sp_spl.rds"
    message("read map from RDS ", rds_map)
    countries_data_map = readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))

    orig_data <-get_datahub() %>%
        get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    # map ----
    # data7_orig_data_aggregate = lw_vars_calc(orig_data_aggregate)
    #
    # # create datasets for maps merging today with data7
    # orig_data_aggregate_maps = orig_data_aggregate %>%
    #   left_join(data7_orig_data_aggregate %>% select(-population))


    callModule(mod_map_server, "map_ui", orig_data_aggregate, countries_data_map)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

