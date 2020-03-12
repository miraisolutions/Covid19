context("get data tests")

test_that("get_timeseries_data has expected headers", {
  data <- get_timeseries_data()
  expect_equal(names(data), c("confirmed", "deaths", "recovered"))
  sapply(names(data), function(i){
    expect_true(all(c("Province.State", "Country.Region", "Lat", "Long") %in% names(data[[i]])))
  })
})
