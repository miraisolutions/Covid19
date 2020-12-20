#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import markdown
#'
#' @noRd
app_server <- function(input, output, session) {

#profvis({
  # Params ----
  n <- 1000 #  min number of cases for a country to be considered. Default 1000
  # to be used in Global and Comparison
  w <- 7 # number of days of outbreak. Default 7

  # Data ----
  # map
  #countries_data_map <- load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))
  rds_map = "WorldMap_sp_rds"
  message("read map from RDS ", rds_map)
  countries_data_map = readRDS(file =  file.path(system.file("./countries_data", package = "Covid19Mirai"),rds_map))

  orig_data_with_ch <- get_datahub_fix_ch()
  orig_data       = orig_data_with_ch$orig_data
  orig_data_ch_2  = orig_data_with_ch$orig_data_ch_2

  orig_data = orig_data %>%
    get_timeseries_by_contagion_day_data()

  orig_data_ch_2 = orig_data_ch_2 %>%
      get_timeseries_by_contagion_day_data()

  pop_data = get_pop_datahub()

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

  output$last_update <- renderText({
    paste0("Latest updated: ",
           max(orig_data$date)
    )
  })


  # Modules ----

  callModule(mod_global_server, "global", orig_data_aggregate = orig_data_aggregate,
             countries_data_map)

  orig_data_aggregate = orig_data_aggregate %>%
              filter(!is.na(continent))

  callModule(mod_continent_comparison_server, "continent_comparison", orig_data_aggregate = orig_data_aggregate, nn = n, w = w, pop_data = pop_data)


  # select continents in tabs
  continents = c("Europe", "Asia", "Africa", "LatAm & Carib.", "Northern America", "Oceania")
  mainuicontinents = c("Europe", "Asia", "Africa", "LatAm", "NorthernAmerica", "Oceania")
  uicontinents = c("europe", "asia", "africa", "latam", "northernamerica", "oceania")
  for (i.cont in 1:length(continents)) {
    callModule(mod_continent_server, paste(mainuicontinents[i.cont], "comparison", sep = "_"),
               orig_data_aggregate = orig_data_aggregate, nn = n, w = w,
               pop_data = pop_data, countries_data_map = countries_data_map,
               cont = continents[i.cont], uicont = uicontinents[i.cont])
  }
  # Switzerland page
  callModule(mod_ind_country_server, "swiss", data = orig_data_aggregate, data2 = orig_data_ch_2, country = "Switzerland", nn = n, w = w)

  # align contagion day for comparisons
  data_filtered <-
    orig_data_aggregate %>%
    rescale_df_contagion(n = n, w = w)

  # determine vector of countries to be used in Global and Comparison pages
  # reactive
  countries <- reactive({
    data_filtered %>%
      select(Country.Region) %>%
      distinct()
  })

  # country choice, remove Switzerland
  orig_data_aggregate_noswiss = orig_data_aggregate %>% filter(Country.Region != "Switzerland")
  countriesnoswiss = reactive({
    countries()[countries()[,1] != "Switzerland",]
  })

  callModule(mod_country_server, "country", data = orig_data_aggregate, countries = countriesnoswiss, nn = n, w = w, n.select = n)

  callModule(mod_country_comparison_server, "country_comparison", data = orig_data_aggregate, countries = countries, nn = 100, w = w, n.select = n)

  # Modal ----
  # what is new pop-up
  observeEvent(input$btn_whatsnew, {
    showModal(modalDialog(
      title = "What's New:",
      includeMarkdown("./NEWS.md"),
      footer = modalButton("Dismiss"),
      size = "l",
      easyClose = TRUE,
      fade = FALSE
    ))
  })
#})

}

