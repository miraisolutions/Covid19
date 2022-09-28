#' Build data in GitHub action yml and save as RDS
#' @rdname get_datahub
#'
#' @import dplyr
#' @export
build_data <- function() {

  message("Start build_data, read Level 1 and CH at Level 2")
  orig_data_with_ch <- get_datahub_fix_ch()
  orig_data <- orig_data_with_ch$orig_data
  orig_data_ch_2 <- orig_data_with_ch$orig_data_ch_2

  orig_data <- orig_data %>%
    get_timeseries_by_contagion_day_data()

  orig_data_ch_2 <- orig_data_ch_2 %>%
    get_timeseries_by_contagion_day_data()

  message("Add pop and map data")
  pop_data <- get_pop_datahub()

  rds_map = "WorldMap_sp_spl.rds"
  message("read map from RDS ", rds_map)
  countries_data_map <- readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))


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
  # remove small countries, population <=1000
  # TODO pop_data = pop_data %>% filter(population >1000)

  orig_data_aggregate <-
    build_data_aggr(orig_data, pop_data)

  message("** Save data as DATA.rds **")
  saveRDS(list(orig_data_aggregate = orig_data_aggregate,
               countries_data_map = countries_data_map,
               pop_data = pop_data,
               orig_data_ch_2 = orig_data_ch_2), "inst/datahub/DATA.rds")

  # read data for default country at level 2
  area_data_2 <- get_datahub(country = .Selected_Country, lev = 2, verbose = FALSE)

  message("** Save data as Selected_Country.rds **")

  saveRDS(list(area_data_2 = area_data_2), "inst/datahub/Selected_Country.rds")

  # take main European countries:
  # pop_data <- get_pop_datahub()
  europe <- pop_data %>% filter(continent == "Europe")

  all_data <- merge_pop_data(orig_data, pop_data)

  top_europe <- all_data %>%
    filter(continent == "Europe") %>%
    distinct(Country.Region,population) %>%
    slice_max(population, n = 25) %>% as.data.frame() %>% .[,"Country.Region"]

  top_counties <- all_data %>% distinct(Country.Region,population) %>%
    slice_max(population, n = 30) %>% as.data.frame() %>% .[,"Country.Region"]

  add_countries <- c("Australia", "Canada", "Argentina", "South Africa", "South Korea")
  top_counties <- c(top_counties, add_countries)
  # remove Switzerland and USA
  all_countries <- union(top_counties, top_europe) %>% setdiff(c("Switzerland", "USA"))

  message("reading data at level 2 for country: ", paste(all_countries, collapse = ","))

  all_lev2_data <- sapply(all_countries, function(cntr) {
    message(cntr)
    orig_data_2_tmp <- get_datahub(country = cntr, lev = 2, verbose = FALSE, cache = TRUE)
    # cant use get_timeseries_by_contagion_day_data because of reconciliation of hosp data
    # if (nrow(orig_data_2_tmp)>0)
    #   orig_data_2_tmp <- orig_data_2_tmp %>%
    #     get_timeseries_by_contagion_day_data()
    orig_data_2_tmp
  })

  # remove those whithout data
  idx <- sapply(1:length(all_lev2_data), function(x)
    nrow(all_lev2_data[[x]])) >0
  message("Level 2 data not found for ", sum(!idx), " countries out of ", length(idx))
  all_lev2_data <- all_lev2_data[idx]

  message("** Save data as Selected_Country.rds **")

  saveRDS(list(area_data_2 = area_data_2), "inst/datahub/Selected_Country.rds")

  message("** Save data as Top_Countries.rds **")

  saveRDS(list(all_lev2_data = all_lev2_data, all_countries = all_countries), "inst/datahub/Top_Countries.rds")
  NULL
}
