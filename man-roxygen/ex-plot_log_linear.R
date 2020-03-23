if (interactive()) {
  library(shiny)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_plot_log_linear_ui("test")
  )
  server <- function(input, output) {

    orig_data <- reactive({
      get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data() %>%
        select(-ends_with("rate"))
    })

    global <- reactive({
      orig_data() %>%
        get_timeseries_global_data() %>%
        select(-ends_with("rate"))
    })

    df_global <- reactive({
      global() %>%
        select(-starts_with("new_")) %>%
        select( -confirmed) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = as.factor(status)) %>%
        capitalize_names_df()
    })

    callModule(mod_plot_log_linear_server,"test", df = df_global, type = "area")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

