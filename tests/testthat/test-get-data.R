context("get data tests")

test_that("get_timeseries_data returns expected headers", {
  data <- get_timeseries_data()
  expect_equal(names(data), c("confirmed", "deaths", "recovered"))
  sapply(names(data), function(i){
    expect_true(all(c("Province.State", "Country.Region", "Lat", "Long") %in% names(data[[i]])))
  })
  sapply(c("confirmed", "deaths", "recovered"), function(i) {
    expect_true(all(c("Province.State", "Country.Region", "Lat", "Long") %in% names(get_timeseries_single_data(i))))
  })
})
# commented out because not used
# test_that("get_daily_data returns expected headers", {
#   data <- get_daily_data('01-22-2020')
#   expect_equal(sort(names(data)), sort(c("Province.State", "Country.Region", "Last.Update", "Confirmed", "Deaths", "Recovered" )))
# })

data_full <- get_timeseries_full_data()

test_that("get_timeseries_full_data returns expected headers", {
  expect_equal(sort(names(data_full)), sort(c("Province.State", "Country.Region", "Lat", "Long", "date", "confirmed", "deaths", "recovered", "active")))
  expect_equal(class(data_full$date),"Date")
})

data <- get_timeseries_by_contagion_day_data(data_full)

test_that("get_timeseries_by_contagion_day_data returns expected headers", {
  expect_equal(sort(names(data)), sort(c("Province.State", "Country.Region", "Lat", "Long", "date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
  expect_equal(class(data$contagion_day),"numeric")
  expect_false(any(is.na(data$contagion_day)))
})

test_that("get_timeseries_global_data returns expected headers", {
  df <- get_timeseries_global_data(data)
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered")))
})

test_that("get_timeseries_country_data returns expected headers", {
  df <- get_timeseries_country_data(data, "Italy")
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})

test_that("get_timeseries_province_data returns expected headers", {
  df <- get_timeseries_province_data(data, "Alaska")
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})

test_that("get_date_data returns expected headers", {
  df <- get_date_data(data, as.Date("2020-01-30"))
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})

test_that("aggregate_country_data returns expected headers", {
  df <- aggregate_country_data(data)
  expect_equal(sort(names(df)), sort(c("Country.Region", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})

test_that("aggregate_province_timeseries_data returns expected headers", {
  df <- aggregate_province_timeseries_data(data)
  expect_equal(sort(names(df)), sort(c("Country.Region", "date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})


test_that("add_growth_death_rate returns expected headers", {
  df <- aggregate_province_timeseries_data(data) %>% add_growth_death_rate()
  expect_true(all(c("growth_factor_3", "growth_factor_5", "growth_factor_7", "lethality_rate") %in% names(df)))
})



pop_data = get_pop_data()
na.pop.data = sum(is.na(pop_data$continent))
# test pop map data
mapdata = load_countries_data_map()

test_that("get_pop_data returns expected rows", {

  orig_data_aggregate = aggregate_province_timeseries_data(data) %>%
    arrange(Country.Region)

  dups = duplicated(orig_data_aggregate[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates

  dups = duplicated(pop_data[, c("Country.Region")])

  expect_true(sum(dups) == 0) # no duplicates

  pop_not_in_data = setdiff(pop_data$Country.Region, orig_data_aggregate$Country.Region)
  # pop_data %>% filter(Country.Region %in% pop_not_in_data)

  # merge with population
  df2 = orig_data_aggregate %>%
    merge_pop_data(pop_data)

  dups = duplicated(df2[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no missing pop

  # countries with NAs population
  missingpop = unique(df2$Country.Region[is.na(df2$population)])

  expect_true(length(missingpop) <= 3, label = paste("Missing population var in data <= 3 fails, value: ", length(missingpop))) # 2 countries do not match population data
  # countries with 0 population
  zeropop = unique(df2$Country.Region[!is.na(df2$population) & df2$population == 0])

  expect_true(length(zeropop) == 0) # no zero pop

  countrynames= unique(orig_data_aggregate$Country.Region)

  countrynames2= unique(df2$Country.Region)

  expect_true(identical(countrynames, countrynames2))

  # diffdf= orig_data_aggregate %>% filter(Country.Region %in% setdiff(countrynames, countrynames2) &
  #                 date == max(date))
  #
  # expect_true(all(diffdf$confirmed <1000))
  # expect_true(all(!is.na(df2$Country.Region))) # no nas
  # expect_true(all(!is.na(df2$population))) # no nas

  na.cont= df2 %>% filter(is.na(continent) &
                          date == max(date))
  na.subcont= df2 %>% filter(is.na(subcontinent) &
                            date == max(date))

  expect_true(all(na.cont$confirmed < 1000))
  expect_true(all(na.subcont$confirmed < 1000))

  # expect_true(nrow(na.cont) == 0)
  # expect_true(nrow(na.subcont) == 0)


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

test_that("aggr_to_cont works correctly", {

  orig_data_aggregate = aggregate_province_timeseries_data(data) %>%
    arrange(Country.Region)
  df = orig_data_aggregate %>%
    merge_pop_data(pop_data)

  # select all variables
  statuses <- c("confirmed", "deaths", "recovered", "active")
  allstatuses = c(statuses, paste0("new_", statuses), "population")

  data_conts = aggr_to_cont(df, group = "continent", time = "date", allstatuses)#, popdata = pop_data)

  dups = duplicated(data_conts[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates
  # matching population
  cont_pop_data =  pop_data %>% filter(!is.na(continent)) %>%
    group_by(continent) %>%
    summarize(population = sum(population, na.rm = T))

  # popcont = tapply(data_conts$population,
  #                  data_conts$Country.Region, unique)
  # cont_pop = cont_pop_data$population
  # names(cont_pop) = cont_pop_data$continent
  # cont_pop = as.array(cont_pop[dimnames(popcont)[[1]]])
  # expect_equal(popcont,
  #              cont_pop)

  europe_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% "Europe") %>%
    group_by(subcontinent) %>%
    summarize(population = sum(population, na.rm = T))

  df_europe = df %>%
    filter(continent == "Europe")

  data_subcont = aggr_to_cont(df_europe, group = "subcontinent", time = "date", allstatuses)#, popdata = europe_pop_data)

  dups = duplicated(data_subcont[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates
  # matching population
  # popsubcont = tapply(data_subcont$population,
  #                     data_subcont$Country.Region, unique)
  # eur_pop = europe_pop_data$population
  # names(eur_pop) = europe_pop_data$subcontinent
  # eur_pop = as.array(eur_pop[dimnames(popsubcont)[[1]]])
  # expect_equal(popsubcont,
  #              eur_pop)

})
