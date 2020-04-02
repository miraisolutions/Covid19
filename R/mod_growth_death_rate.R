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
             div(h4("Current top 5 countries growth factor"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             radioButtons(inputId = ns("growth_rate"), label = "",
                          choices = list("Over 3 days" = "growth_rate_3",
                                         "Over 5 days" = "growth_rate_5",
                                         "Over one week" = "growth_rate_7"),
                          selected = "growth_rate_3", inline = TRUE),
             withSpinner(uiOutput(ns("plot_growth_rate")))
      ),
      column(6,
             div(h4("Current top 5 countries death rate"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             div(style = "visibility: hidden",
                 radioButtons(inputId = ns("dummy"), label = "",
                              choices = list("dummy" = "dummy"),
                              selected = "dummy", inline = TRUE),
                 ),
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
  n <- 1000 #min number of cases for a country to be considered
  w <- 7 #min lenght of outbreak
  n_highligth <- 5 # top countries

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
      arrange(desc(Value)) %>%
      top_n(n_highligth, wt = Value) %>%
      mutate(Country = as.factor(Country.Region)) %>%
      select(Country, Value)

    df_plot
  }


  df_base_plot1 <- reactive({pick_rate_hist( req(df()), input$growth_rate)})
  df_base_plot2 <- reactive({pick_rate_hist( req(df()), "death_rate")})

  # Plots ----
  caption <- reactive({list(
    growth_rate = paste0("Computed as total confirmed cases today / total confirmed cases ", gsub("growth_rate_", "", input$growth_rate) ," days ago."),
    death_rate = "Computed as total deaths today / total confirmed cases."
  )})

  output$plot_growth_rate <- renderUI({
    tagList(
      plotlyOutput(ns("plot_growth_rate_hist"), height = 400),
      div(p(caption()[["growth_rate"]]), align = "center"),
      div(p(paste0("Only countries with more than ", n, " confirmed cases and outbreaks longer than ", w, " days considered.")), align = "center"),
    )
  })

  output$plot_death_rate <- renderUI({
    tagList(
      plotlyOutput(ns("plot_death_rate_hist"), height = 400),
      div(p(caption()[["death_rate"]]), align = "center"),
      div(p(paste0("Only countries with more than ", n, " confirmed cases and outbreaks longer than ", w, " days considered.")), align = "center"),
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
