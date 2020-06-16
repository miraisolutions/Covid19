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
#' @importFrom shinycssloaders withSpinner
mod_compare_nth_cases_plot_ui <- function(id){
  ns <- NS(id)
  choices_plot <- c(names(case_colors), "new_confirmed", "new_active", "growth_factor_3", "lethality_rate") %>%
    setNames(gsub("_", " ",c(names(case_colors), "new_confirmed", "new_active", "growth_factor_3", "lethality_rate"))) %>% as.list()
  # UI ----
  tagList(
    uiOutput(ns("title")),
    fluidRow(
      column(7,
             offset = 1,
             radioButtons(inputId = ns("radio_indicator"), label = "",
                          choices = choices_plot, selected ="confirmed", inline = TRUE)
      ),
      column(4,
             radioButtons(inputId = ns("radio_log_linear"), label = "",
                          choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)
      )
    ),
    withSpinner(plotlyOutput(ns("plot"), height = 400)),
    div(uiOutput(ns("caption")), align = "center")
  )
}

#' compare_nth_cases_plot Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#' @param   n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param n_highligth number of countries to highlight
#' @param istop logical to choose title
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
mod_compare_nth_cases_plot_server <- function(input, output, session, orig_data_aggregate, n = 1000, w = 7, n_highligth = 5, istop = T){
  ns <- session$ns

  # Data ----
  #This only depends on the orig_data_aggregate
  df_clean <- reactive({
    df_clean <- orig_data_aggregate() %>%
      # select(-starts_with("new_")) %>%
      rescale_df_contagion(n = n, w = w)
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
      filter(Date == max(Date)) %>%
      filter(Value ==  max(Value)) %>%
      ungroup() %>%
      arrange(desc(Value)) %>%
      top_n(n_highligth, wt = Value)

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

    p <- plot_all_highlight(df(), log = log(), text = "Country", n_highligth = n_highligth, percent = ifelse(input$radio_indicator == "death_rate", T, F), date_x = F)

    p <- p %>%
      plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p

  })

  if (istop) {
    output$title <- renderUI({
      div(h4(paste0("Top ",n_highligth," countries from day of ", n ," contagion")), align = "center", style = "margin-top:20px; margin-bottom:20px;")
    })
  } else {
    output$title <- renderUI({
      div(h4(paste0("Timeline from day of ", n ," contagion")), align = "center", style = "margin-top:20px; margin-bottom:20px;")
    })
  }

  output$caption <- renderUI({
      p(paste0("Computed as rolling weekly average. Considering countries with at least ", n," confirmed cases, and outbreaks longer than ",w," days. Day 0 is the day the country reached ", n," confirmed cases. Notice that China has been cut off to the second longest outbreak."))
  })

}

