#' growth_death_rate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_growth_death_rate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             div(h4("Current top 5 countries growth rate"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             withSpinner(uiOutput(ns("plot_growth_rate")))
      ),
      column(6,
             div(h4("Current top 5 countries death rate"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             withSpinner(uiOutput(ns("plot_death_rate")))
      )
    )
  )
}

#' growth_death_rate Server Function
#'
#' @param df reactive data.frame
#'
#' @import dplyr
#' @import tidyr
#'
#' @example ex-mod_growth_death_rate.R
#'
#' @noRd
mod_growth_death_rate_server <- function(input, output, session, df){
  ns <- session$ns

  # Params ----
  n <- 10000 #min number of cases for a country to be considered
  w <- 7 #min lenght of outbreak
  N <- 5 # top countries

  caption <- list(
    growth_rate = "Computed as number of days it took double the number of confirmed cases.",
    death_rate = "Computed as total deaths today / total confirmed cases yesterday. "
  )

  # Help funcs ----

  pick_rate <- function(orig_data_aggregate, rate){
    df <-  orig_data_aggregate  %>%
      select(-starts_with("new_")) %>%
      bind_cols(orig_data_aggregate[, rate] %>% setNames("Value"))
    df
  }

  pick_rate_hist <- function(orig_data_aggregate, rate){
    df_plot <- orig_data_aggregate %>%
      select_countries_n_cases_w_days(n = n, w = w) %>%
      pick_rate(rate) %>%
      filter( date == max(date)) %>%
      mutate(Country = as.factor(Country.Region)) %>%
      select(Country, Value)
    df_plot
  }


  df_base_plot1 <- reactive({pick_rate_hist( req(df()), "growth_rate") %>% top_n(N, wt = desc(Value))})
  df_base_plot2 <- reactive({pick_rate_hist( req(df()), "death_rate") %>% top_n(N, wt = Value)})

  # Plots ----

  output$plot_growth_rate <- renderUI({
    tagList(
      br(),
      div(p(caption[["growth_rate"]]), align = "center"),
      div(p(paste0("Only countries with more than ", n, " cases and outbreaks longer than ", w, " days considered.")), align = "center"),
      br(),
      plotlyOutput(ns("plot_growth_rate_hist"), height = 400)
    )
  })

  output$plot_death_rate <- renderUI({
    tagList(
      br(),
      div(p(caption[["death_rate"]]), align = "center"),
      div(p(paste0("Only countries with more than ", n, " cases and outbreaks longer than ", w, " days considered.")), align = "center"),
      br(),
      plotlyOutput(ns("plot_death_rate_hist"), height = 400)
    )
  })

  output$plot_growth_rate_hist <- renderPlotly({
    p <- plot_rate_hist(df_base_plot1(), color =  "growth_rate")
    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })

  output$plot_death_rate_hist <- renderPlotly({
    p <- plot_rate_hist(df_base_plot2(), color =  "death_rate", percent = T)
    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })

}
