if (interactive()) {
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_vaccines_text_ui("vaccines_text")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    # orig_data <- get_datahub(country = "Switzerland") %>%
    #   get_timeseries_by_contagion_day_data()

    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    orig_data_aggregate = orig_data_aggregate %>%
      filter(Country.Region == "Italy")

    country_data <-
      orig_data_aggregate %>%
        #filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))

    lw_country_data =  lw_vars_calc(country_data)

    country_data_today = country_data %>%
      add_growth_death_rate()
    country_data_today = country_data_today  %>%
      left_join(lw_country_data %>% select(-population))

    callModule(mod_vaccines_text_server,"vaccines_text", dftoday = country_data_today, df = country_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

