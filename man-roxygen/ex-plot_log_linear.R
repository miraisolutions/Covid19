if (interactive()) {
  #devtools::load_all()

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_plot_log_linear_ui("test", select = FALSE, area = FALSE)
  )
  server <- function(input, output) {

    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data
    n = 100
    w = 7
    data_filtered <-
      orig_data %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)

    country_data <- reactive({data_filtered %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })

    country <- reactive({
      country_data() %>%
        get_timeseries_global_data()
    })

    levs <- areaplot_vars()

    df_country = reactive({
      tsdata_areplot(country(),levs)
    })

    callModule(mod_plot_log_linear_server,"test", df = df_country(), type = "line")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test", select = FALSE, area = TRUE)
  )
  server <- function(input, output) {

    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    n = 100
    w = 7
    data_filtered <-
      orig_data %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <- reactive({data_filtered %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })

    country <- reactive({
      country_data() %>%
        get_timeseries_global_data()
    })

    levs <- areaplot_vars()

    df_country = reactive({
      tsdata_areplot(country(),levs)
    })

    callModule(mod_plot_log_linear_server,"test", df = df_country(), type = "area")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
# with select
if (interactive()) {
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test", select = TRUE, area = TRUE)
  )
  server <- function(input, output) {

    orig_data <- get_datahub() %>%
      get_timeseries_by_contagion_day_data()

    n = 100
    w = 7
    data_filtered <-
      orig_data %>%
      Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <- data_filtered %>%
        #filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))

    levs <- areaplot_vars()

    df_area = purrr::map(unique(country_data$Country.Region),
                           function(un) {
                             dat = tsdata_areplot(country_data[country_data$Country.Region == un, ], levs, nn = 10) #n = 0 for area plot
                             dat$Country.Region = rep(un, nrow(dat))
                             dat
                           })
    df_country = Reduce("rbind",df_area)

    countries <- reactive({
      country_data %>%
        select(Country.Region) %>%
        distinct() #%>% .$Country.Region
    })
    callModule(mod_plot_log_linear_server,"test", df = df_country, type = "area", countries = countries)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


# with select and hospedalised data
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test", select = TRUE, area = TRUE)
  )
  server <- function(input, output) {

    orig_data_ch2 <- get_datahub(country = "India", lev = 2) %>%
      get_timeseries_by_contagion_day_data()

    n = 1
    w = 1
    data_filtered <-
      orig_data_ch2 %>%
      Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <- data_filtered %>%
      #filter(Country.Region %in% "Switzerland") %>%
      filter(contagion_day > 0) %>%
      arrange(desc(date))

    levs <- areaplot_hospvars()

    df_area = purrr::map(unique(country_data$Country.Region),
                         function(un) {
                           dat = tsdata_areplot(country_data[country_data$Country.Region == un, ], levs, nn = 10) #n = 0 for area plot
                           dat$Country.Region = rep(un, nrow(dat))
                           dat
                         })
    df_country = Reduce("rbind",df_area)

    countries <- reactive({
      country_data %>%
        select(Country.Region) %>%
        distinct() #%>% .$Country.Region
    })
    callModule(mod_plot_log_linear_server,"test", df = df_country, type = "area", countries = countries, hosp = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


# with select and hospedalised data
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test", select = TRUE, area = TRUE)
  )
  server <- function(input, output) {

    orig_data_I <- get_datahub(country = "India", lev = 1) %>%
      get_timeseries_by_contagion_day_data()

    n = 1
    w = 1
    data_filtered <-
      orig_data_I %>%
      Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <- data_filtered %>%
      #filter(Country.Region %in% "Switzerland") %>%
      filter(contagion_day > 0) %>%
      arrange(desc(date))

    levs <- areaplot_hospvars()

    df_area = purrr::map(unique(country_data$Country.Region),
                         function(un) {
                           dat = tsdata_areplot(country_data[country_data$Country.Region == un, ], levs, nn = 10) #n = 0 for area plot
                           dat$Country.Region = rep(un, nrow(dat))
                           dat
                         })
    df_country = Reduce("rbind",df_area)

    countries <- reactive({
      country_data %>%
        select(Country.Region) %>%
        distinct() #%>% .$Country.Region
    })
    callModule(mod_plot_log_linear_server,"test", df = df_country, type = "area", countries = countries, hosp = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


