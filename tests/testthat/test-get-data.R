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


test_that("get_pop_data returns expected rows", {

  orig_data_aggregate = aggregate_province_timeseries_data(data) %>%
    arrange(Country.Region) %>%
    align_country_names_pop()
  dups = duplicated(orig_data_aggregate[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates

  dups = duplicated(pop_data[, c("Country.Region")])

  expect_true(sum(dups) == 0) # no duplicates
  df2 = orig_data_aggregate %>%
    merge_pop_data(pop_data) %>%
    align_country_names_pop_reverse()

  dups = duplicated(df2[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates

  df = orig_data_aggregate %>%
    align_country_names_pop_reverse()

  countrynames= unique(df$Country.Region)

  countrynames2= unique(df2$Country.Region)

  diffdf= df %>% filter(Country.Region %in% setdiff(countrynames, countrynames2) &
                  date == max(date))

  expect_true(all(diffdf$confirmed <1000))
  expect_true(all(!is.na(df2$Country.Region))) # no nas
  expect_true(all(!is.na(df2$population))) # no nas

  na.cont= df2 %>% filter(is.na(continent) &
                          date == max(date))
  na.subcont= df2 %>% filter(is.na(subcontinent) &
                            date == max(date))
  expect_true(nrow(na.cont) == 0)

  expect_true(nrow(na.subcont) == 0)

})

test_that("aggr_to_cont works correctly", {

  orig_data_aggregate = aggregate_province_timeseries_data(data) %>%
    arrange(Country.Region) %>%
    align_country_names_pop()
  df = orig_data_aggregate %>%
    merge_pop_data(pop_data) %>%
    align_country_names_pop_reverse()

  statuses <- c("confirmed", "deaths", "recovered", "active")
  # select all variables
  allstatuses = c(statuses, paste0("new_", statuses))

  data_conts = aggr_to_cont(df, group = "continent", time = "date", popdata = pop_data, allstatuses)

  dups = duplicated(data_conts[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates
  # matching population
  cont_pop_data =  pop_data %>% filter(!is.na(continent)) %>%
    group_by(continent) %>%
    summarize(population = sum(population, rm.na = T))

  popcont = tapply(data_conts$population,
                   data_conts$Country.Region, unique)
  cont_pop = cont_pop_data$population
  names(cont_pop) = cont_pop_data$continent
  cont_pop = as.array(cont_pop[dimnames(popcont)[[1]]])
  expect_equal(popcont,
               cont_pop)

  europe_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% "Europe") %>%
    group_by(subcontinent) %>%
    summarize(population = sum(population, rm.na = T))

  df_europe = df %>%
    filter(continent == "Europe")

  data_subcont = aggr_to_cont(df_europe, group = "subcontinent", time = "date", popdata = europe_pop_data, allstatuses)

  dups = duplicated(data_subcont[, c("Country.Region", "date")])

  expect_true(sum(dups) == 0) # no duplicates
  # matching population
  popsubcont = tapply(data_subcont$population,
                      data_subcont$Country.Region, unique)
  eur_pop = europe_pop_data$population
  names(eur_pop) = europe_pop_data$subcontinent
  eur_pop = as.array(eur_pop[dimnames(popsubcont)[[1]]])
  expect_equal(popsubcont,
               eur_pop)

})
