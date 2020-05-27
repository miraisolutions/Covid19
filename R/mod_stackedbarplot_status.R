#' stackedbarplot_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_stackedbarplot_ui <- function(id, n_highligth = 5){
  ns <- NS(id)
  tagList(
          uiOutput(ns("title_stackedbarplot_status")),
          withSpinner(uiOutput(ns("plot_stackedbarplot_status")))
      )
}
#' stackedbarplot_status Server Function
#'
#' @param countries_data reactive data.frame for multiple countries
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import purrr
#' @importFrom plotly ggplotly layout
#' @noRd
mod_stackedbarplot_status_server <- function(input, output, session, df, n = 1000, w = 7, n_highligth = 5, istop = T){
  ns <- session$ns
  # titles
  if (istop) {
    output$title_stackedbarplot_status <- renderUI(div(h4(paste0("Current top ", n_highligth, " status split")), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  } else {
    output$title_stackedbarplot_status <- renderUI(div(h4("Status split by country"), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  }

  prep_data <- function(orig_data_aggregate, n, w){
    df1 <- orig_data_aggregate %>%
      Covid19:::select_countries_n_cases_w_days(n = n, w = w) %>%
      filter( date == max(date)) %>%
      Covid19:::align_country_names_pop() %>%
      mutate(country_name = Country.Region) %>%
      Covid19:::get_pop_data() %>%
      filter(population > 10^6) %>% # dropping countries with less than 1 M pop, needed?
      Covid19:::align_country_names_pop_reverse()
    df1
  }
  df_pop <- reactive({prep_data(df(), n,w)})
  #df_pop = prep_data(df, n = 1000, w = 7)

  statuses <- c("deaths", "active", "recovered")

  pick_status <- function(df, stat){
    df <-  df  %>%
      bind_cols(df[, stat] %>% setNames("Value"))
    df
  }
  df_status = reactive({pick_status(df_pop(), "confirmed") %>%
    arrange(desc(Value)) %>%
    top_n(n_highligth, wt = Value)  %>%
    select(Country.Region,!!statuses)})


  # gather status and compute ratios
  df_status_stack = reactive({df_status() %>%
      gather("status", "countstatus", -Country.Region) %>%
    group_by(Country.Region) %>%
    mutate(n.pop = sum(countstatus),
           ratio.over.cases  = countstatus/n.pop) %>%
    group_by(status) %>%
    mutate(tot.status = sum(countstatus),
           ratio.status  = countstatus/tot.status) %>%
    ungroup() %>%
    mutate(Country.Region = as.factor(Country.Region),
           status = factor(status, levels = statuses)) %>%
    arrange(status)})

  caption_explain <- "Status split per country as of today."

  output$plot_stackedbarplot_status <- renderUI({
    tagList(
      plotlyOutput(ns("plot_stackedbarplot_status_draw"), height = 400),
      div(p(caption_explain), align = "center")
    )
  })

  output$plot_stackedbarplot_status_draw <- renderPlotly({
    p = df_status_stack() %>%
        stackedbarplot_plot() %>% fix_colors()
    p <- p %>%
      ggplotly(tooltip = c("text", "fill"))   %>%
      layout(legend = list(orientation = "v", y = 1, yanchor = "left"))

    p$x$data <-
      p$x$data %>%
      purrr::map(clean_plotly_leg, "[^\\(][^,]*")

    p
  })
}
