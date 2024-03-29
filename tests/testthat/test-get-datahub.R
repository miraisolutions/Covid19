context("get datahub tests")

data_full_ch <- get_datahub_fix_ch()
data_full = data_full_ch$orig_data
data_ch2 = data_full_ch$orig_data_ch_2
vars = c("Country.Region", "date","confirmed", "deaths","active", "recovered", "tests",
         "population","stringency_index",.vacc_vars_datahub,.hosp_vars)

check_data <- function(dat, vars) {
  if (!missing(vars))
    expect_true(length(setdiff(names(dat), vars)) == 0)
  expect_false(any(sapply(dat, class) == "integer"))
  expect_true(all(dat$confirmed >= dat$recovered, na.rm = TRUE))
  expect_true(all(dat$confirmed >= dat$deaths, na.rm = TRUE))
  expect_true(all(dat$confirmed >= (dat$recovered + dat$deaths), na.rm = TRUE))
}
test_that("get_datahub returns expected headers and variables", {

  check_data(data_full, vars)
  check_data(data_ch2, vars)

  # cumalitive vars are cumulative, not always true due to corrections
  #expect_true(all(data_full %>% dplyr::group_by(Country.Region) %>% diff(confirmed), na.rm = TRUE))
  #conftest = data_full %>% dplyr::group_by(Country.Region) %>% summarize(check = all(diff(confirmed) >=0)) %>% ungroup()

})
test_that("get_datahub does not return data from today", {
  expect_false(identical(Sys.Date(),max(data_full$date)))
})
if (FALSE) {
  test_that("get_datahub contains Hong Kong", {
    expect_true("Hong Kong" %in% unique(data_full$Country.Region))
  })
}

##############
# test last week on swiss data

test_that("get_datahub lev = 2 Swiss is ok", {
  data_ch2 = get_timeseries_by_contagion_day_data(data_ch2)
  orig_data_aggregate_ch2 = build_data_aggr(data_ch2)

  lw_orig_data_aggregate_ch2 =  lw_vars_calc(orig_data_aggregate_ch2)

  orig_data_aggregate_ch2_today = orig_data_aggregate_ch2 %>%
    add_growth_death_rate()
  if (nrow(lw_orig_data_aggregate_ch2) > 0) {
    orig_data_aggregate_ch2_today = orig_data_aggregate_ch2_today  %>%
      left_join(lw_orig_data_aggregate_ch2 %>% select(-population))
  }

  # no NAs, without considering test data, they are not provided anymore
  expect_equal(sum(sapply(orig_data_aggregate_ch2_today %>% select(!contains("test")), is.na)), 0)
  check_data(data_ch2)
  check_data(orig_data_aggregate_ch2)
  check_data(orig_data_aggregate_ch2_today)
})

# French data give always problems
data_FR <- get_datahub("France", lev = 2)

test_that("get_datahub lev = 2 France is ok", {
  check_data(data_FR, vars)
})

# HK not found anymore in china lev2 apparently
if (FALSE) {
  data_HK <- get_datahub("Hong Kong", lev = 1)

  test_that("get_datahub lev = 1 Hong Kong works", {
    expect_true("Hong Kong" %in% unique(data_HK$Country.Region))
    check_data(data_HK, vars)

  })
}


data <- get_timeseries_by_contagion_day_data(data_full)
vars = c(vars,"AsOfDate")

test_that("get_timeseries_by_contagion_day_data returns expected headers", {

  check_data(data)
  varnames = as.vector(c("confirmed", "deaths", "recovered", "active","tests",.vacc_vars_datahub, .hosp_vars))
  expect_equal(sort(names(data)), as.vector(sort(c("Country.Region", "date","AsOfDate",varnames ,"population","stringency_index",
                                         paste0("new_",varnames), "contagion_day"))))
  expect_equal(class(data$contagion_day),"numeric")
  expect_false(any(sapply(data, class) == "integer"))

  expect_false(any(is.na(data$contagion_day)))
})


test_that("add_growth_death_rate returns expected headers", {
  df <- data %>% add_growth_death_rate()
  check_data(df)
  expect_true(all(c("growth_factor_3", "growth_factor_14", "growth_factor_7") %in% names(df)))
})

test_that("test for missing dates and today data", {
  df <- data %>% group_by(Country.Region) %>%
    mutate(lagdate = as.integer(date) - lag(as.integer(date)))

  expect_true(length(table(df$lagdate))== 1)
  expect_true(names(table(df$lagdate))== "1")

  df = data %>% filter(date %in% tail(date,1)) %>% select(Country.Region, date, new_confirmed)
  expect_true(sum(df$new_confirmed) != 0) # there are data (there could be negatives)

})
test_that("test for missing or duplicated countries", {
  expect_true(sum(is.na(data$Country.Region)) == 0)

  dups = duplicated(data[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates

  # countries with 0 population
  zeropop = unique(data$Country.Region[!is.na(data$population) & data$population == 0])

  expect_true(length(zeropop) == 0) # no zero pop
})
pop_data = get_pop_datahub()
na.pop.data = sum(is.na(pop_data$continent))
# test pop map data
# mapdata = load_countries_datahub_map()

rds_map = "WorldMap_sp_spl.rds"
# message("read map from RDS ", rds_map)
mapdata <- readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))


test_that("test pop_data and build_data_aggr returns expected format", {

  dups = duplicated(pop_data[, c("Country.Region")])

  expect_true(sum(dups) == 0) # no duplicates

  na.cnt = pop_data$Country.Region[is.na(pop_data$continent)]
  na.scnt = pop_data$Country.Region[is.na(pop_data$subcontinent)]

  expect_true(length(na.cnt) == 5)
  expect_equal(length(na.cnt), length(na.scnt))
  # some countries seem now to be among those with no continent. To be fixed
  #expect_false(any(unique(data$Country.Region) %in% na.cnt))

  orig_data_aggregate <- build_data_aggr(data, pop_data)

  countrynames= unique(orig_data_aggregate$Country.Region)

  countrynames2= unique(data$Country.Region)

  expect_true(identical(sort(countrynames), sort(countrynames2)))


  na.cont= orig_data_aggregate %>% filter(is.na(continent) &
                          date == max(date))
  na.subcont= orig_data_aggregate %>% filter(is.na(subcontinent) &
                            date == max(date))

  # expect_true(all(na.cont$confirmed < 1000))
  # expect_true(all(na.subcont$confirmed < 1000))
  # some countries seem now to be among those with no continent. To be fixed
  # expect_false(any((sort(na.cont$Country.Region) %in% # none of the missing countries is a valid country
  #                       c(pop_data$Country.Region)))) #

  # check differences with other data
  setdiff(mapdata$NAME, pop_data$Country.Region)
  setdiff(pop_data$Country.Region, mapdata$NAME)

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
  res = .align_map_pop(mapdata, pop_data)
  pop_data = res$pop
  mapdata = res$map

  setdiff(mapdata$NAME, pop_data$Country.Region)
  setdiff(pop_data$Country.Region, mapdata$NAME)

  expect_true(sum(is.na(pop_data$continent)) <=  na.pop.data )

})
