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

test_that("get_daily_data returns expected headers", {
  data <- get_daily_data('01-22-2020')
  expect_equal(sort(names(data)), sort(c("Province.State", "Country.Region", "Last.Update", "Confirmed", "Deaths", "Recovered" )))
})

data_full <- get_timeseries_full_data()

test_that("get_timeseries_full_data returns expected headers", {
  expect_equal(sort(names(data_full)), sort(c("Province.State", "Country.Region", "Lat", "Long", "date", "confirmed", "deaths", "recovered", "active")))
  expect_equal(class(data_full$date),"Date")
})

data <- get_timeseries_by_contagion_day_data(data_full)

test_that("get_timeseries_by_contagion_day_data returns expected headers", {
  expect_equal(sort(names(data)), sort(c("Province.State", "Country.Region", "Lat", "Long", "date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate", "contagion_day")))
  expect_equal(class(data$contagion_day),"numeric")
  expect_false(any(is.na(data$contagion_day)))
})

test_that("get_timeseries_global_data returns expected headers", {
  df <- get_timeseries_global_data(data)
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate")))
})

test_that("get_timeseries_country_data returns expected headers", {
  df <- get_timeseries_country_data(data, "Italy")
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate", "contagion_day")))
})

test_that("get_timeseries_province_data returns expected headers", {
  df <- get_timeseries_province_data(data, "Alaska")
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate", "contagion_day")))
})

test_that("get_date_data returns expected headers", {
  df <- get_date_data(data, as.Date("2020-01-30"))
  expect_equal(sort(names(df)), sort(c("date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate", "contagion_day")))
})

test_that("aggregate_country_data returns expected headers", {
  df <- aggregate_country_data(data)
  expect_equal(sort(names(df)), sort(c("Country.Region", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
})

test_that("aggregate_province_timeseries_data returns expected headers", {
  df <- aggregate_province_timeseries_data(data)
  expect_equal(sort(names(df)), sort(c("Country.Region", "date", "confirmed", "deaths", "recovered", "active","new_confirmed", "new_deaths", "new_active", "new_recovered", "growth_rate", "death_rate", "contagion_day")))
})
