if (interactive()) {


  #cont = "Asia"
 #sapply(file.path("R",list.files("R")), source)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_map_cont_ui("map_cont_ui")
    )
  )
  server <- function(input, output) {
    country = "Switzerland"

    area_data_2 <- get_datahub(country, lev = 2) %>%
        get_timeseries_by_contagion_day_data()

    area_data_2_aggregate <-
      build_data_aggr(area_data_2)
    countries_data_map = leaflet::gadmCHE

    subcont_palette =
      palette_calc(col_cont = area_map_spec(country, "col"),
                           x = sort(unique(area_data_2_aggregate$Country.Region)))


    callModule(mod_map_cont_server, "map_cont_ui", orig_data_aggregate = area_data_2_aggregate,  countries_data_map,
               area = country, g_palette = subcont_palette)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

