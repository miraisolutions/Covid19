
########
# pop data
require(dplyr)
population <- read.csv2(system.file("population_data/pop.csv", package = "Covid19"),stringsAsFactors = F) %>%
  select(-X)
# rename_country.region <- function(countryregion){
#   recode(countryregion,
#     "Ivory Coast" = "C\\u00f4te d'Ivoire",
#     "DR Congo" = "Republic of the Congo",
#     "United Arab Emirates" = "UAE",
#     "East Timor" = "Timor-Leste" ,
#     "Saint Vincent and the Grenadines" = "St. Vincent Grenadines",
#     "Puerto Rico(US)" = "Puerto Rico",
#     "Comoros" = "Mayotte",
#     "Guam(US)" = "Guam",
#     "Greenland(Denmark)" = "Greenland",
#     "Eswatini" = "eSwatini",
#     "Isle of Man(UK)" = "Channel Islands",
#     "Central African Republic" = "CAR",
#     "Cape Verde" = "Cabo Verde",
#     "Antigua and Barbuda" = "Antigua and Barb.",
#     "United States" = "United States of America"
#   )
# }

# https://population.un.org/wpp/DataQuery/
#library("XLConnect")
# library("readxl") # to be used off line

if (FALSE) {

  POP <- read_excel(system.file("population_data/AnnualTotPopMidYear-20200530044226.xlsx", package = "Covid19")
                    , range = "Data!A2:D264") %>% as.data.frame()

  POPDEATHS <- read_excel(system.file("population_data/NumberDeaths-20200530044710.xlsx", package = "Covid19")
                          , range = "Data!A2:Z235") %>% as.data.frame()


  POPAGE = read_excel(system.file("population_data/PopulationAgeSex-20200530044916.xlsx", package = "Covid19")
                      , range = "Data!A2:Z235") %>% as.data.frame()

  POPDENS = read_excel(system.file("population_data/PopulationDensity-20200530043909.xlsx", package = "Covid19")
                       , range = "Data!A2:D269") %>% as.data.frame()

  POPURBAN = read_excel(system.file("population_data/AnnualUrbanPopMidYear-20200530081243.xlsx", package = "Covid19")
                        , range = "Data!A2:D264") %>% as.data.frame()

  POPRURAL = read_excel(system.file("population_data/AnnualRuralPopMidYear-20200530081402.xlsx", package = "Covid19")
                        , range = "Data!A2:D264") %>% as.data.frame()

  setdiff(population$Country.Region, POP$Location)

  rename_location <- function(loc){
    newloc = recode(loc,
                    #"French Guiana" = "Guyana",
                    "Russian Federation" = "Russia" ,   "United States of America" = "United States",
                    "Viet Nam" = "Vietnam",           "Democratic Republic of the Congo" = "DR Congo",
                    "Iran (Islamic Republic of)" = "Iran"  ,     "United Republic of Tanzania" = "Tanzania",
                    "Republic of Korea" =  "South Korea",     "Venezuela (Bolivarian Republic of)" = "Venezuela",
                    "Côte d'Ivoire" =  "Ivory Coast",   "Dem. People's Republic of Korea" = "North Korea",
                    "China, Taiwan Province of China" =  "Taiwan",           "Syrian Arab Republic" = "Syria",
                    "Bolivia (Plurinational State of)" =  "Bolivia","Dominican Republic" = "Dominican Rep.",
                    "China, Hong Kong SAR" =  "Hong Kong",     "Lao People's Democratic Republic" = "Laos",
                    "State of Palestine" =  "Palestine",     "Bosnia and Herzegovina" = "Bosnia and Herz.",
                    "Puerto Rico" =  "Puerto Rico(US)" ,     "Republic of Moldova" = "Moldova",
                    "TFYR Macedonia" =  "Macedonia",      #"Kosovo",
                    "Swaziland" = "Eswatini",
                    "Timor-Leste" =  "East Timor",    #"Eswatini",
                    "China, Macao SAR" =  "Macao",     "Cabo Verde" = "Cape Verde",
                    #"Transnistria",
                    "Brunei Darussalam" = "Brunei",     # "Cyprus" =  "Northern Cyprus"     ,
                    "New Caledonia"  = "New Caledonia(France)",           "French Polynesia" =  "French Polynesia(France)"  ,
                    #"Abkhazia",
                    "Sao Tome and Principe" =  "São Tomé and Príncipe",           "Guam" = "Guam(US)",
                    "Curaçao" =  "Curaçao(Netherlands)" ,       #"Artsakh",
                    "Aruba" =  "Aruba(Netherlands)" ,           #"Jersey(UK)",
                    "United States Virgin Islands" =  "U.S. Virgin Islands(US)" ,           "Micronesia (Fed. States of)" = "F.S. Micronesia",
                    "Isle of Man" =  "Isle of Man(UK)"         ,           "Cayman Islands" = "Cayman Islands(UK)",
                    "Bermuda" =  "Bermuda(UK)",           #"Guernsey(UK)",
                    "American Samoa" =  "American Samoa(US)" ,           "Greenland"  = "Greenland(Denmark)",
                    "Northern Mariana Islands" =  "Northern Mariana Islands(US)",           #"South Ossetia",
                    "Faeroe Islands" = "Faeroe Is.",           "Turks and Caicos Islands" = "Turks and Caicos Islands(UK)",
                    "Sint Maarten (Dutch part)" =  "Sint Maarten(Netherlands)",           #"Saint Martin(France)",
                    "Gibraltar" =  "Gibraltar(UK)",           "British Virgin Islands"  = "British Virgin Islands(UK)",
                    # "Åland Islands(Finland)" ,
                    "Cook Islands" = "Cook Islands(NZ)",           "Anguilla" =  "Anguilla(UK)",
                    #"Wallis and Futuna(France)",           #"St-Barthélemy",
                    "Saint Pierre and Miquelon" = "Saint Pierre and Miquelon(France)",           "Saint Helena" =  "Saint Helena, Ascension",
                    #"and Tristan da Cunha(UK)",
                    "Montserrat" =  "Montserrat(UK)",           "Falkland Islands (Malvinas)" = "Falkland Islands(UK)",
                    # "Christmas Island(Australia)",           #"Norfolk Island(Australia)"
                    "Niue" =  "Niue(NZ)",           "Tokelau" = "Tokelau(NZ)",
                    # "Vatican",           #"Cocos (Keeling) Islands(Australia)",
                    # "Pitcairn Islands(UK)"
    )
    newloc
  }

  # rename to latest????
  if(F){
    population$Country.Region <- rename_country.region(population$Country.Region)
    POP$Location = rename_country.region(POP$Location)
    POPAGE$Location = rename_country.region(POPAGE$Location)
    POPDENS$Location = rename_country.region(POPDENS$Location)
    POP$Location = rename_country.region(POP$Location)
  }


  continents = c("Africa", "Asia", "Europe", "Latin America and the Caribbean",
                 #    "South America",
                 "Northern America", "Oceania")


  sub_continents= list(
    "Africa" =  c("Eastern Africa",  "Middle Africa" ,  "Northern Africa",
                  "Southern Africa", "Western Africa"),
    "Asia" = c("Eastern Asia",
               "South-Central Asia" ,      "Central Asia"  ,    "Southern Asia",
               "South-Eastern Asia" ,                    "Western Asia"),
    "Europe"  = c( "Eastern Europe" ,  "Northern Europe",
                   "Southern Europe",      "Western Europe"),
    "Latin America and the Caribbean" = c("Caribbean",  "Central America" ,  "South America"
    ),
    "Northern America"  = c("Bermuda(UK)", "Canada","Greenland(Denmark)","Saint Pierre and Miquelon(France)" ,"United States"),
    "Oceania"=  c("Australia/New Zealand","Australia","New Zealand",
                  "Melanesia","Micronesia","Polynesia")
  )
  sub_continents_vect_int =  as.vector(unlist(sub_continents))
  cont_data = data.frame(continent =  rep(names(sub_continents), sapply(sub_continents, length)),
                         subcontinent = sub_continents_vect_int)

  for( cont in continents) {
    pos.append = match(head(sub_continents[[cont]],1), sub_continents_vect_int)
    sub_continents_vect_int = append(sub_continents_vect_int, cont,
                                     pos.append-1)
  }

  sub_continents_vect_int = as.vector(sub_continents_vect_int)

  group_countries <- function(pop) {
    pop$Location = rename_location(pop$Location)
    pop = pop[,!grepl("^ISO",names(pop))]
    pop = pop[,setdiff(names(pop), c("Time","Note"))]

    pop = pop %>% filter(Location != "World") %>%
      rename("PopulationUN" = '2020') %>%
      mutate(PopulationUN = PopulationUN * 1000)
    pop = pop[, setdiff(names(pop), "Note")]
    # add continent
    rep.tc = diff(match(continents, pop$Location))
    rep.tc = c(rep.tc, nrow(pop)- sum(rep.tc))
    continentslist = rep(continents,
                         rep.tc)
    pop$continent = continentslist
    # add sub continent
    rep.tsc = diff(match(sub_continents_vect_int, pop$Location))
    rep.tsc[is.na(rep.tsc)] = 1
    rep.tsc = c(rep.tsc, nrow(pop)- sum(rep.tsc))
    subcontinentslist = rep(sub_continents_vect_int,
                            rep.tsc)
    pop$subcontinent = subcontinentslist
    pop
  }

  POP =  group_countries(POP)


  clean_pop_age = function(pop){
    pop$Location = rename_location(pop$Location)
    pop = pop[,!grepl("^ISO",names(pop))]
    pop = pop[, setdiff(names(pop),c( "Note","Sex", "Time"))]

    names(pop)[grep("[0-9]-[0-9]", names(pop))] = paste0("Pop_",names(pop)[grep("[0-9]-[0-9]", names(pop))] )
    pop = pop %>% filter(Location != "World")
    pop
  }
  POPAGE = clean_pop_age(POPAGE)

  POP = dplyr::left_join(POP, POPAGE, by = "Location")

  clean_pop_dens = function(pop){
    pop$Location = rename_location(pop$Location)
    pop = pop[,setdiff(names(pop), c("Time","Note"))]
    pop = pop[,!grepl("^ISO",names(pop))]
    names(pop)[grep("[0-9]-[0-9]", names(pop))] = paste0("Pop_",names(pop)[grep("[0-9]-[0-9]", names(pop))] )
    pop = pop %>% filter(Location != "World") %>%
      rename("Density_per_km2" = '2020')
    pop
  }

  POPDENS =  clean_pop_dens(POPDENS)

  POP = dplyr::left_join(POP, POPDENS, by = "Location")


  clean_pop_deaths = function(pop){
    pop$Location = rename_location(pop$Location)
    pop = pop[,c("Location", "Total")]
    pop = pop %>% filter(Location != "World") %>%
      rename("Deaths" = 'Total') %>%
      mutate(Deaths = Deaths * 1000)
    pop
  }

  POPDEATHS =  clean_pop_deaths(POPDEATHS)
  POP = dplyr::left_join(POP, POPDEATHS, by = "Location")

  clean_pop_urban = function(pop){
    pop$Location = rename_location(pop$Location)
    pop = pop[,setdiff(names(pop), c("Time","Note"))]
    pop = pop[,!grepl("^ISO",names(pop))]
    pop = pop %>% filter(Location != "World") %>%
      rename("Urban_Pop" = '2020') %>%
      mutate(Urban_Pop = Urban_Pop * 1000)

    pop
  }

  POPURBAN = clean_pop_urban(POPURBAN)

  POP = dplyr::left_join(POP, POPURBAN, by = "Location")

  clean_pop_rural = function(pop){
    pop$Location = rename_location(pop$Location)
    pop = pop[,setdiff(names(pop), c("Time","Note"))]
    pop = pop[,!grepl("^ISO",names(pop))]
    pop = pop %>% filter(Location != "World") %>%
      rename("Rural_Pop" = '2020') %>%
      mutate(Rural_Pop = Rural_Pop * 1000)

    pop
  }

  POPRURAL = clean_pop_rural(POPRURAL)

  POP = dplyr::left_join(POP, POPRURAL, by = "Location")


  final_population = dplyr::left_join(population, POP, by = c("Country.Region" = "Location")) %>%
    mutate(PopulationUN = if_else(is.na(PopulationUN), as.integer(population), as.integer(PopulationUN)))

  final_population %>%
    filter(is.na(continent))

  # assign to continent and subcontinnent those missings

  missingEurope = c("Kosovo" = "Southern Europe",
                    "Northern Cyprus" = "Southern Europe",
                    "Jersey(UK)" = "Channel Islands",
                    "Guernsey(UK)" = "Channel Islands",
                    "Vatican" = "Southern Europe",
                    "Transnistria" = "Eastern Europe",
                    "Åland Islands(Finland)" = "Northern Europe")
  missingAsia = c("Abkhazia" = "Western Asia"
                  , "South Ossetia" = "Western Asia",
                  "Artsakh" = "Western Asia"
  )
  missingOceania = grep("Australia", final_population$Country.Region, value = T)
  missingOceania = intersect(missingOceania,final_population$Country.Region[is.na(final_population$continent)] )
  missingAfrica = c("Eswatini" = "Southern Africa")
  missingAmerica = c("Eswatini" = "Southern Africa")

  final_population = final_population %>%
    mutate(continent = if_else(is.na(continent) &
                                 Country.Region %in% names(missingEurope), "Europe", continent)) %>%
    mutate(continent = if_else(is.na(continent) &
                                 Country.Region %in% names(missingAsia), "Asia", continent)) %>%
    mutate(continent = if_else(is.na(continent) &
                                 Country.Region %in% missingOceania, "Oceania", continent)) %>%
    mutate(continent = if_else(is.na(continent) &
                                 Country.Region %in% names(missingAfrica), "Africa", continent))

  sub.na = is.na(final_population$subcontinent) & final_population$Country.Region %in% names(missingEurope)
  final_population$subcontinent[sub.na] = missingEurope

  sub.na = is.na(final_population$subcontinent) & final_population$Country.Region %in% names(missingAsia)
  final_population$subcontinent[sub.na] = missingAsia

  sub.na = is.na(final_population$subcontinent) & final_population$Country.Region %in% missingOceania
  final_population$subcontinent[sub.na] = "Australia"
  # # adjust micronesia
  # final_population$subcontinent[final_population$Country.Region == "F.S. Micronesia"] = "Micronesia"

  sub.na = is.na(final_population$subcontinent) & final_population$Country.Region %in% names(missingAfrica)
  final_population$subcontinent[sub.na] = missingAfrica


  final_population %>%
    filter(is.na(subcontinent))

  if (any(duplicated(final_population$Country.Region)))
    stop("generated uplicates")
  if (length(setdiff(unique(population$Country.Region), unique(final_population$Country.Region)))>0)
    stop("not the same countries as before")

  # to regroup in case
  newcontinents  = c("Africa", "Asia", "Europe", "Lat. America & Carib",
                     "South America", "Northern America", "Oceania")

  final_population = final_population %>%
    mutate(continent = if_else(continent == "Latin America and the Caribbean", "LatAm & Carib.", continent))
  write.table(final_population,
              file.path("inst/population_data","popUN.csv" ),row.names = FALSE,
              sep = ";")


}

if (FALSE) {

  #' load countries data to match with get_datahub
  #' @param destpath path to file
  #'
  #' @returns countries shapefile
  #' @noRd
  load_countries_datahub_map <- function(destpath = system.file("./countries_data", package = "Covid19Mirai")){
    # Resource https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/
    url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
    zip_path <- file.path(destpath,"ne_50m_admin_0_countries.zip")
    dsn_path <- file.path(destpath, "ne_50m_admin_0_countries")

    if (!file.exists(zip_path)) {
      download.file(url = url, destfile = zip_path)
      unzip(zip_path, exdir = dsn_path)
    }

    countries <- rgdal::readOGR(dsn = dsn_path,
                                layer = "ne_50m_admin_0_countries",
                                encoding = "utf-8", use_iconv = T,
                                verbose = FALSE)
    assign_new_level =  function(countrymap , lev, from, to, regexpress = FALSE) {
      if (regexpress) {
        if (!any(grepl(from, as.character(countrymap[[lev]]))))
          stop("wrong expression, not present in map: ", from)
        from = grep(from, as.character(countrymap[[lev]]), value = TRUE)
      }
      if (!any(countrymap[[lev]] == from))
        stop("wrong name, not present in map: ", from)
      levels(countrymap[[lev]])[levels(countrymap[[lev]])==from] <- to
      countrymap[[lev]][countrymap[[lev]] == from] = to
      countrymap
    }
    #rename continents
    countries = assign_new_level(countries, "CONTINENT", "North America", "Northern America")
    countries = assign_new_level(countries, "CONTINENT", "South America", "LatAm & Carib.")
    #rename NAME, i.e. countries
    #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
    #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
    countries = assign_new_level(countries, "NAME", "Macedonia", "North Macedonia")
    countries = assign_new_level(countries, "NAME", "Czechia", "Czech Republic")
    countries = assign_new_level(countries, "NAME", "Dominican Rep.", "Dominican Republic")
    countries = assign_new_level(countries, "NAME", "United Kingdom", "UK")
    countries = assign_new_level(countries, "NAME", "United States of America", "USA")
    #countries = assign_new_level(countries, "NAME", "United Arab Emirates", "UAE") # now has new name
    countries = assign_new_level(countries, "NAME", "^St-Barth", "St. Barth", regexpress = TRUE) # not needed but ok
    countries = assign_new_level(countries, "NAME", "Faeroe Is.", "Faeroe Islands")
    countries = assign_new_level(countries, "NAME", "Bosnia and Herz.", "Bosnia and Herzegovina")
    countries = assign_new_level(countries, "NAME", "Vatican", "Vatican City")
    countries = assign_new_level(countries, "NAME", "St. Vin. and Gren.", "St. Vincent Grenadines")
    countries = assign_new_level(countries, "NAME", "Dem. Rep. Congo", "Republic of the Congo")
    #countries = assign_new_level(countries, "NAME", "Central African Rep.", "CAR")
    countries = assign_new_level(countries, "NAME", "Ivoire", "Cote d'Ivoire", regexpress = TRUE)
    countries = assign_new_level(countries, "NAME", "St-Martin", "St Martin")
    countries = assign_new_level(countries, "NAME", "Cayman Is.", "Cayman Islands")
    countries = assign_new_level(countries, "NAME", "Eq. Guinea", "Equatorial Guinea")
    countries = assign_new_level(countries, "NAME", "Central African Rep.", "Central African Republic")
    countries = assign_new_level(countries, "NAME", "eSwatini", "Swaziland")
    countries = assign_new_level(countries, "NAME", "Cabo Verde", "Cape Verde")
    countries = assign_new_level(countries, "NAME", "S. Sudan", "South Sudan")
    countries = assign_new_level(countries, "NAME", "Fr. Polynesia", "French Polynesia")
    countries = assign_new_level(countries, "NAME", "Antigua and Barb.", "Antigua and Barbuda")
    countries = assign_new_level(countries, "NAME", "Cook Is.", "Cook Islands")
    countries = assign_new_level(countries, "NAME", "Falkland Is.", "Falkland Islands")
    countries = assign_new_level(countries, "NAME", "U.S. Virgin Is.", "U.S. Virgin Islands") # new name
    countries = assign_new_level(countries, "NAME", "St. Kitts and Nevis", "Saint Kitts and Nevis" ) # new name
    countries = assign_new_level(countries, "NAME", "Principe", "Sao Tome and Principe", regexpress = TRUE )
    countries = assign_new_level(countries, "NAME", "N. Mariana Is.", "Northern Mariana Islands")
    countries = assign_new_level(countries, "NAME", "Marshall Is.", "Marshall Iselands")
    countries = assign_new_level(countries, "NAME", "St. Pierre and Miquelon", "St. Pierre and Miquelon")

    countries
  }
}

