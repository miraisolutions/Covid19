if (interactive()) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  #sapply(file.path("R",list.files("R")), source)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test")
  )
  server <- function(input, output) {

    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })

    n = 100
    w = 7
    data_filtered <- reactive({
      orig_data() %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)
    })

    country_data <- reactive({data_filtered() %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })

    country <- reactive({
      country_data() %>%
        get_timeseries_global_data()
    })

    levs <- sort_type_hardcoded()

    df_country = reactive({
      tsdata_areplot(country(),levs)
    })



    callModule(mod_plot_log_linear_server,"test", df = df_country, type = "area")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  #sapply(file.path("R",list.files("R")), source)

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    Covid19Mirai:::mod_plot_log_linear_ui("test")
  )
  server <- function(input, output) {

    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })
    n = 100
    w = 7
    data_filtered <- reactive({
      orig_data() %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)
    })

    country_data <- reactive({data_filtered() %>%
        filter(Country.Region %in% "Switzerland") %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })

    country <- reactive({
      country_data() %>%
        get_timeseries_global_data()
    })

    levs <- sort_type_hardcoded()

    df_country = reactive({
      tsdata_areplot(country(),levs)
    })



    callModule(mod_plot_log_linear_server,"test", df = df_country, type = "area")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

