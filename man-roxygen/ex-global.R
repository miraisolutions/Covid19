if (interactive()) {
  #devtools::load_all()


  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_global_ui("global")
  )
  server <- function(input, output) {

    rds_map = "WorldMap_sp_spl.rds"
    message("read map from RDS ", rds_map)
    countries_data_map = readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    pop_data = get_pop_datahub()

    #align continents from map with pop
    #country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(pop_data$Country.Region, unique(as.character(countries_data_map$NAME)))])
    .align_map_pop <- function(map,pop) {
      tmp = map@data[,c("NAME","CONTINENT")] %>%
        merge(pop[,c("Country.Region","continent")], by.x = "NAME", by.y = "Country.Region", all.x = T, sort = FALSE, incomparables = NA)
      tmp = tmp[match(map@data$NAME,tmp$NAME),]
      tmp2 = pop[,c("Country.Region","continent")] %>%
        merge(map@data[,c("NAME","CONTINENT")], by.x = "Country.Region", by.y = "NAME", all.x = T, sort = FALSE, incomparables = NA)
      tmp2 = tmp2[match(pop$continent,tmp2$continent),]

      map@data$CONTINENT[!is.na(tmp$continent)] = tmp$continent[!is.na(tmp$continent)]
      pop$continent[is.na(pop$continent)] = as.character(tmp2$CONTINENT[is.na(pop$continent)])

      list(map = map, pop = pop)
    }

    res = .align_map_pop(countries_data_map, pop_data)
    pop_data = res$pop
    countries_data_map = res$map

    callModule(mod_global_server, "global",
               orig_data_aggregate, countries_data_map)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

