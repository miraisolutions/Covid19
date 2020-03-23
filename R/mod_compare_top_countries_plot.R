#' compare_top_countries_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
mod_compare_top_countries_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      radioButtons(inputId = ns("radio_indicator"), label = "",
                   choices = names(case_colors), selected = names(case_colors)[1], inline = TRUE),
      plotlyOutput(ns("plot"), height = 400)
    )
  )
}

#' compare_top_countries_plot Server Function
#'
#' @param global reactive data.frame
#'
#' @example ex-mod_compare_top_countries_plot.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @import dplyr
#' @import tidyr
#' @import ggplot
#'
#' @noRd
mod_compare_top_countries_plot_server <- function(input, output, session, orig_data){
  ns <- session$ns

  # Data ----
  df_clean <- reactive({
    orig_data() %>%
      select(-c(Province.State, Lat, Long, contagion_day), -starts_with("new_")) %>%
      group_by(Country.Region, date) %>%
      summarise_each(sum) %>%
      ungroup() %>%
      arrange(desc(Country.Region), date) %>%
      mutate(no_contagion = case_when(
        confirmed < 100 ~ 1,
        TRUE ~ 0
      )) %>%
      group_by(Country.Region) %>%
      mutate(incremental = seq(1:n())) %>%
      mutate(offset = sum(no_contagion)) %>%
      mutate(tmp = incremental - offset) %>%
      mutate(contagion_day = case_when(
        tmp < 0 ~ 0,
        TRUE ~ tmp
      )) %>%
      filter(contagion_day >= 1) %>%
      select(-c(incremental, offset, tmp))
  })

  df <- reactive({
    df_clean() %>%
      bind_cols(df_clean()[,input$radio_indicator] %>% setNames("Value")) %>%
      mutate(Date = contagion_day) %>%
      mutate(Status = Country.Region ) %>%
      group_by(Status) %>%
      mutate(sel1 = ifelse(max(Date < 7), 1, 0)) %>%
      filter(sel1 == 0) %>%
      mutate(sel2 = ifelse(max(Value < 1000), 1, 0)) %>%
      filter(sel2 == 0) %>%
      select(Status, Value, Date)
  })

  # Plot -----
  output$plot <- renderPlotly({

    p <- ggplot(df(), aes(x = Date, y = Value, colour = Status, text = paste0("Country: ", Status))) +
      geom_line(size = 1) +
      basic_plot_theme() +
      scale_colour_brewer(palette = "Dark2")

    p <- p %>%
      ggplotly(tooltip = c("x", "y", "text")) %>%
      layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))

  })

}

