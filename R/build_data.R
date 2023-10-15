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

  # data for global module

  total <-
    orig_data_aggregate %>%
    get_timeseries_global_data() %>% mutate(Country.Region = "World") %>%
    get_timeseries_by_contagion_day_data()

  total_aggregate <- total %>%  # add additional vars
    build_data_aggr()

  total_today <-
    total_aggregate %>%
    filter(date == AsOfDate)
  lw_total =  lw_vars_calc(total_aggregate)
  pw_total =  lw_vars_calc(total_aggregate, 14)

  total_today = total_today  %>%
    left_join(lw_total %>% select(-population))  %>%
    left_join(pw_total %>% select(-population))

  orig_data_aggregate = orig_data_aggregate %>%
    filter(population > 300000) # remove very small countries
  # countries today
  orig_data_aggregate_today <-
    orig_data_aggregate %>%
    add_growth_death_rate()

  lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
  pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate, 14)

  orig_data_aggregate_today = orig_data_aggregate_today  %>%
    left_join(lw_orig_data_aggregate %>% select(-population)) %>%
    left_join(pw_orig_data_aggregate %>% select(-population))
  # TODO: REVIEW!
  world <-
    orig_data_aggregate_today %>%
    arrange(desc(confirmed) )
  # todp 5 countries today by confirmed
  world_top_5_today <-
    world %>%
    head(5)

  world_top_5_confirmed <-
    orig_data_aggregate %>%
    filter(Country.Region %in% world_top_5_today$Country.Region) %>%
    select(Country.Region, date, AsOfDate,confirmed)

  TOTAL <- list(
    world_top_5_confirmed = world_top_5_confirmed,
    world_top_5_today = world_top_5_today,
    world = world,
    orig_data_aggregate_today = orig_data_aggregate_today,
    total = total,
    total_aggregate = total_aggregate,
    total_today = total_today
  )

  # Data for continent comparison module

  # aggregate data to continent
  message("Data for continent comparison module")
  continent_data <- aggr_to_cont(orig_data_aggregate %>% filter(!is.na(continent)), "continent", "date")

  continents = unique(continent_data$Country.Region)

  nn <- 1000; w <- 7
  # create data for comparison with common starting point
  continent_data_filtered <- continent_data %>%
    rescale_df_contagion(n = nn, w = w)

  continent_data_filtered_today = continent_data_filtered %>%
    add_growth_death_rate()

  lw_continent_data_filtered =  lw_vars_calc(continent_data_filtered)
  pw_continent_data_filtered =  lw_vars_calc(continent_data_filtered, 14)

  continent_data_filtered_today = continent_data_filtered_today  %>%
    left_join(lw_continent_data_filtered %>% select(-population))  %>%
    left_join(pw_continent_data_filtered %>% select(-population))

  CONTINENTS <- list(
    continent_data = continent_data, continent_data_filtered_today = continent_data_filtered_today, continent_data_filtered = continent_data_filtered
  )

  # DATA for one continent;

  build_continent <- function(cont) {
    message("Build Continent ", cont)
    orig_data_aggregate_cont <-
      orig_data_aggregate %>% filter(continent == cont)

    # subcontinents = reactive({sort(unique(orig_data_aggregate_cont$subcontinent))})
    subcontinents = sort(unique(orig_data_aggregate_cont$subcontinent))

    continent_data <-
      aggr_to_cont(orig_data_aggregate_cont, "continent", "date" )

    subcontinent_data <-
      aggr_to_cont(orig_data_aggregate_cont, "subcontinent", "date" )

    subcontinent_data_filtered <-
      subcontinent_data %>% # select sub-continents with longer outbreaks
      rescale_df_contagion(n = nn, w = w)

    subcontinent_data_filtered_today = subcontinent_data_filtered %>%
      add_growth_death_rate()

    lw_subcontinent_data_filtered =  lw_vars_calc(subcontinent_data_filtered)
    pw_subcontinent_data_filtered =  lw_vars_calc(subcontinent_data_filtered, 14)

    subcontinent_data_filtered_today = subcontinent_data_filtered_today  %>%
      left_join(lw_subcontinent_data_filtered %>% select(-population))  %>%
      left_join(pw_subcontinent_data_filtered %>% select(-population))


    continent_data_today <-
      continent_data %>%
      filter(date == AsOfDate)
    lw_continent_data_today =  lw_vars_calc(continent_data)
    pw_continent_data_today =  lw_vars_calc(continent_data, 14)

    continent_data_today = continent_data_today  %>%
      left_join(lw_continent_data_today %>% select(-population))  %>%
      left_join(pw_continent_data_today %>% select(-population))

    # Compute Last week variables
    data7_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont)
    data14_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont, 14)

    orig_data_aggregate_cont_today = orig_data_aggregate_cont %>%
      add_growth_death_rate()

    # scatterplot

    # remove small countries
    countries200000 = sort(unique(orig_data_aggregate_cont_today$Country.Region[orig_data_aggregate_cont_today$population > 200000]))

    # create datasets for maps merging today with data7
    data_cont_maps = orig_data_aggregate_cont_today  %>%
      left_join(data7_aggregate_cont %>% select(-population))  %>%
      left_join(data14_aggregate_cont %>% select(-population))

    list(continent_data_today = continent_data_today, continent_data = continent_data,
         subcontinent_data = subcontinent_data, subcontinent_data_filtered = subcontinent_data_filtered,
         subcontinent_data_filtered_today = subcontinent_data_filtered_today,
         data_cont_maps = data_cont_maps)
  }
  continents <- unique(orig_data_aggregate$continent[!is.na(orig_data_aggregate$continent)])

  ONE.CONTINENT <- lapply(continents, build_continent) %>% setNames(continents)

  message("** Save data as DATA.rds **")
  saveRDS(list(orig_data_aggregate = orig_data_aggregate,
               countries_data_map = countries_data_map,
               pop_data = pop_data,
               orig_data_ch_2 = orig_data_ch_2,
               TOTAL = TOTAL, CONTINENTS = CONTINENTS, ONE.CONTINENT = ONE.CONTINENT), "inst/datahub/DATA.rds")

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
