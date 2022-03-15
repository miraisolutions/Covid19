if (interactive()) {
  devtools::load_all()

  cont = "LatAm & Carib."
  cont = "Europe"

  variable = "death rate" # set variable
  #variable = "prevalence rate" # set variable
  variable = "actives" # set variable
  #variable = "tests" # set variable
  variable = "positive tests rate" # set variable
  #variable = "hospitalised" # set variable
  #variable = "hospitalized over 1M" # set variable
  #variable = "stringency_index" # set variable
 # variable = "growth vs stringency" # set variable
 #  variable = "growth vs prevalence" # set variable
  #variable = "vaccines" # set variable
 #
 variable = "growth factor" # set variable
  # variable = "confirmed" # set variable

  #sapply(file.path("R",list.files("R")), source)
  #pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_map_area_calc_ui("map_cont_calc_ui")
    )
  )
  server <- function(input, output) {

    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    # orig_data = readRDS("orig_data.rds")
    pop_data = get_pop_datahub()
    orig_data_aggregate =  build_data_aggr(orig_data, pop_data)

    #countries_data_map <- Covid19Mirai:::load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))
    rds_map = "WorldMap_sp_spl.rds"
    message("read map from RDS ", rds_map)
    countries_data_map = readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))

    #countries_data_map = rmapshaper::ms_simplify(countries_data_map, keep = 0.4)
    orig_data_aggregate_cont <-
      orig_data_aggregate %>% filter(continent == cont)

    data7_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont)

    # create datasets for maps merging today with data7
    data_cont_maps = orig_data_aggregate_cont %>% add_growth_death_rate() %>%
      left_join(data7_aggregate_cont %>% select(-population))

    .subsetmap = function(map,cc) {
      idx = map$CONTINENT %in% cc
      countries = map$NAME[idx]
      map_cont = subset(map, NAME %in% countries, drop = T)
      map_cont$CONTINENT = factor(map_cont$CONTINENT)
      map_cont$NAME = factor(map_cont$NAME)
      map_cont
    }
    countries_data_map_cont = .subsetmap(countries_data_map, cc = cont)

    callModule(mod_map_area_calc_server, "map_cont_calc_ui", df = data_cont_maps,
               countries_data_map_cont, variable = variable, area = cont)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

