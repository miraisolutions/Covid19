if (interactive()) {
  #devtools::load_all()

  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("plot_log_area_global")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <-
      data_filtered %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))

    levs <- areaplot_vars()

    df_tot <- reactive({
      country_data %>%
        #select(-Country.Region, -contagion_day) %>%
        select(date, !!levs) %>%
        #select(-starts_with("new"), -confirmed, -starts_with("growth_"), -ends_with("_rate"), -contains("1M")) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = factor(status, levels = levs)) %>%
        capitalize_names_df()
    })


    callModule(mod_plot_log_linear_server,"plot_log_area_global", df = df_tot(), type = "area")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

