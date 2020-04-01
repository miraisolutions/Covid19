#' compare_nth_cases_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
mod_compare_nth_cases_plot_ui <- function(id){
  ns <- NS(id)
  # Params ----
  N <- 10000 #number of cases for comparison
  # UI ----
  tagList(
    div(h4(paste0("Top 5 countries from day of ", N," contagion")), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
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
    div(p(paste0("Showing countries with at least ", N," cases, and outbreaks longer than a week.")), align = "center"),
    div(p(paste0("Notice that China has been cut off to the second longest outbreak.")), align = "center")
  )
}

#' compare_nth_cases_plot Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#'
#' @example ex-mod_compare_nth_cases_plot.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_compare_nth_cases_plot_server <- function(input, output, session, orig_data_aggregate){
  ns <- session$ns

  # Params ----
  N <- 10000 #number of cases for comparison

  # Data ----
  #This only depends on the orig_data_aggregate
  df_clean <- reactive({
    df_clean <- orig_data_aggregate() %>%
      select(-starts_with("new_"), -ends_with("_rate")) %>%
      mutate(no_contagion = case_when(
        confirmed < N ~ 1,
        TRUE ~ 0
      )) %>%
      filter(no_contagion == 0) %>% # pick only those countries that had at least N cases
      group_by(Country.Region) %>%
      mutate(tmp = contagion_day - min(contagion_day)) %>%
      mutate(N = n()) %>%
      ungroup() %>%
      filter( N > 7) %>% #pick only those countries that have had outbreak for more than one week
      mutate(contagion_day = tmp) %>%
      select( -c(tmp, no_contagion, N))
    df_clean
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

    # Day of the country with max contagions after china
    max_contagion_no_china <- countries %>%
      filter(Status != "China") %>%
      filter(Date == max(Date)) %>%
      select(Date) %>%
      as.numeric()

    df <- df_tmp %>%
      filter(Status %in% as.vector(countries$Status)) %>% #pick only filtered countries
      filter(Date <= max_contagion_no_china) %>% #cut china
      mutate(Status = factor(Status, levels = as.vector(countries$Status))) #order by factor

    df
  })

  log <- reactive({
    input$radio_log_linear != "linear"
  })

  # Plot -----
  output$plot <- renderPlotly({

    p <- plot_all_highlight(df(), log = log(), text = "Country", n_highligth = 5)

    p <- p %>%
      plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p

  })

}

