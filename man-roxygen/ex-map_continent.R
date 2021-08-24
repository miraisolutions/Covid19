if (interactive()) {
 # devtools::load_all()

  cont = "Africa"
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_map_cont_ui("map_cont_ui")
    )
  )
  server <- function(input, output) {

    orig_data <- get_datahub() %>%
        get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    #countries_data_map <- Covid19Mirai:::load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))
    rds_map = "WorldMap_sp_spl.rds"
    message("read map from RDS ", rds_map)
    countries_data_map = readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))


    orig_data_aggregate_cont <-
      orig_data_aggregate %>% filter(continent == cont)


    subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) #%>%

    subcont_palette =
      subcont_palette_calc(col_cont = area_map_spec(cont, "col"),
                           x = sort(unique(c(subcontinent_pop_data$subcontinent,
                                             orig_data_aggregate_cont$subcontinent))))


    callModule(mod_map_cont_server, "map_cont_ui", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map,
               cont = cont, g_palette = subcont_palette)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

