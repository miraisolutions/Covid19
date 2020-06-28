context("get data tests")

data_full <- get_datahub()

test_that("get_datahub returns expected headers", {
  expect_true(length(setdiff(names(data_full), c("Country.Region", "date", "confirmed", "deaths","active", "recovered", "tests","population","hosp"))) == 0)
  expect_false(any(sapply(data_full, class) == "integer"))

  # sapply(names(data), function(i){
  #   expect_true(all(c("Province.State", "Country.Region", "Lat", "Long") %in% names(data[[i]])))
  # })
  # sapply(c("confirmed", "deaths", "recovered"), function(i) {
  #   expect_true(all(c("Province.State", "Country.Region", "Lat", "Long") %in% names(get_timeseries_single_data(i))))
  # })
})
test_that("get_datahub returns data from today", {

  expect_true(identical(Sys.Date()-1,max(data_full$date)))

})

data <- get_timeseries_by_contagion_day_data(data_full)

test_that("get_timeseries_by_contagion_day_data returns expected headers", {
  expect_equal(sort(names(data)), sort(c("Country.Region", "date", "tests", "confirmed", "deaths", "recovered", "active", "hosp", "population",
                                         "new_confirmed", "new_deaths", "new_active", "new_recovered", "contagion_day")))
  expect_equal(class(data$contagion_day),"numeric")
  expect_false(any(sapply(data, class) == "integer"))

  expect_false(any(is.na(data$contagion_day)))
})


test_that("add_growth_death_rate returns expected headers", {
  df <- data %>% add_growth_death_rate()
  expect_true(all(c("growth_factor_3", "growth_factor_5", "growth_factor_7", "lethality_rate") %in% names(df)))
})

test_that("test for missing dates and today data", {
  df <- data %>% group_by(Country.Region) %>%
    mutate(lagdate = as.integer(date) - lag(as.integer(date)))

  expect_true(length(table(df$lagdate))== 1)
  expect_true(names(table(df$lagdate))== "1")

  df = data %>% filter(date %in% tail(date,1)) %>% select(Country.Region, date, new_confirmed)
  expect_true(sum(df$new_confirmed) > 0) # there are data

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
mapdata = load_countries_datahub_map()

test_that("test pop_data and that build_data_aggr returns expected format", {

  dups = duplicated(pop_data[, c("Country.Region")])

  expect_true(sum(dups) == 0) # no duplicates

  na.cnt = pop_data$Country.Region[is.na(pop_data$continent)]
  na.scnt = pop_data$Country.Region[is.na(pop_data$subcontinent)]

  expect_true(length(na.cnt) == 5)
  expect_equal(length(na.cnt), length(na.scnt))
  expect_false(any(unique(data$Country.Region) %in% na.cnt))


  orig_data_aggregate <- build_data_aggr(data, pop_data)

  countrynames= unique(orig_data_aggregate$Country.Region)

  countrynames2= unique(data$Country.Region)

  expect_true(identical(sort(countrynames), sort(countrynames2)))


  na.cont= orig_data_aggregate %>% filter(is.na(continent) &
                          date == max(date))
  na.subcont= orig_data_aggregate %>% filter(is.na(subcontinent) &
                            date == max(date))

  expect_true(all(na.cont$confirmed < 1000))
  expect_true(all(na.subcont$confirmed < 1000))
  expect_false(any(!(sort(na.cont$Country.Region) %in% # none of the missing countries is outside the 3 ships
                        c("Diamond Princess", "Grand Princess", "MS Zaandam" )))) # the 3 ships



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
