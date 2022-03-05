#' Build data in GutHub action yml and save as RDS
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

  message("** Save data as DATA.rds **")
  saveRDS(list(orig_data = orig_data, orig_data_ch_2 = orig_data_ch_2), "inst/datahub/DATA.rds")

  # read data for default country at level 2
  area_data_2 <- get_datahub(country = .Selected_Country, lev = 2, verbose = FALSE)

  # take main European countries:
  pop_data <- get_pop_datahub()
  europe <- pop_data %>% filter(continent == "Europe")

  all_data <- merge_pop_data(orig_data, pop_data)

  top_europe <- all_data %>%
    filter(continent == "Europe") %>%
    distinct(Country.Region,population) %>%
    slice_max(population, n = 25) %>% as.data.frame() %>% .[,"Country.Region"]

  top_counties <- all_data %>% distinct(Country.Region,population) %>%
    slice_max(population, n = 25) %>% as.data.frame() %>% .[,"Country.Region"]

  add_countries <- c("Australia", "Canada", "Argentina", "South Africa", "South Korea")
  top_counties <- c(top_counties, add_countries)
  # remove Switzerland and USA
  all_countries <- union(top_counties, top_europe) %>% setdiff(c("Switzerland", "USA"))

  message("reading data at level 2 for country: ", paste(all_countries, collapse = ","))

  all_lev2_data <- sapply(all_countries, function(cntr) {
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
