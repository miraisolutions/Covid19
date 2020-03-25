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
      fluidRow(
        column(7,
               offset = 1,
               radioButtons(inputId = ns("radio_indicator"), label = "",
                            choices = names(case_colors), selected = names(case_colors)[1], inline = TRUE)
        ),
        column(4,
               radioButtons(inputId = ns("radio_log_linear"), label = "",
                            choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)
        )
      ),
      plotlyOutput(ns("plot"), height = 400),
      p("Showing countries with at least 1000 cases and outbreaks linger than a week.", style = "margin-left:50px;")
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
#' @import ggplot2
#'
#' @noRd
mod_compare_top_countries_plot_server <- function(input, output, session, orig_data){
  ns <- session$ns

  # Data ----
  #This only depends on the orig_data
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
      select(-c(incremental, offset, tmp, -no_contagion)) %>%
      ungroup()
  })



  # Give DF standard structure; reacts to input$radio_indicator
  df <- reactive({
    df_tmp <- df_clean() %>%
      bind_cols(df_clean()[,input$radio_indicator] %>% setNames("Value")) %>%
      mutate(Status = Country.Region ) %>%
      mutate(Date = contagion_day ) %>%
      select(Status, Value, Date)

    # Countries listed by their max value
    countries <- df_tmp %>%
      group_by(Status) %>%
      filter(Value == max(Value)) %>%
      filter(Date == max(Date)) %>%
      ungroup() %>%
      arrange(desc(Value))

    #pick only those countries that have had more than 1000 cases and the outpbreak for more than one week
    countries_filtered <- countries %>%
      group_by(Status) %>%
      filter(Date > 7) %>%
      filter(Value > 1000) %>%
      ungroup() %>%
      arrange(desc(Value))

    # Day of the country with max contagions after china
    max_contagion_no_china <- countries_filtered$Date[2] %>% as.numeric()

    df <- df_tmp %>%
      filter(Status %in% as.vector(countries_filtered$Status)) %>% #pick only filtered countries
      filter(Date <= max_contagion_no_china) %>% #cut china
      mutate(Status = factor(Status, levels = as.vector(countries_filtered$Status))) #order by factor

    df
  })

  log <- reactive({
    input$radio_log_linear != "linear"
  })

  # Plot -----
  output$plot <- renderPlotly({

    p <- plot_all_highlight_10(df(), log = log(), text = "Country")

    p <- p %>%
      plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p

  })

}

