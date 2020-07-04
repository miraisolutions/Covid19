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
  choices_plot <- c(names(case_colors)[!grepl("hosp",names(case_colors))],
                    "new_confirmed", "new_active", "growth_factor_3", "lethality_rate") %>%
    setNames(gsub("_", " ",c(names(case_colors)[!grepl("hosp",names(case_colors))],
                             "new_confirmed", "new_active", "growth_factor_3", "lethality_rate"))) %>% as.list()
  # UI ----
  tagList(
    uiOutput(ns("title")),
    fluidRow(
      column(7,
             offset = 1,
             selectInput(inputId = ns("radio_indicator"), label = "",
                          choices = choices_plot, selected ="active")
      ),
      column(4,
             selectInput(inputId = ns("radio_log_linear"), label = "",
                          choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
              # radioButtons(inputId = ns("radio_log_linear"), label = "",
              #            choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)
      )
    ),
    withSpinner(plotlyOutput(ns("plot"), height = 400)),
    div(uiOutput(ns("caption")), align = "center")
  )
}

#' compare_nth_cases_plot Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param n_highligth number of countries to highlight
#' @param istop logical to choose title
#' @param g_palette character vector of colors for the graph and legend
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
mod_compare_nth_cases_plot_server <- function(input, output, session, orig_data_aggregate,
                                              n = 1000, w = 7,
                                              n_highligth = 5, istop = T, g_palette = graph_palette){
  ns <- session$ns

  # Give DF standard structure; reacts to input$radio_indicator
  df <- reactive({

    if(istop) {
      countries_order =  orig_data_aggregate %>% filter(date == max(date)) %>%
        arrange(desc(!!as.symbol(input$radio_indicator))) %>%
        #arrange(!!as.symbol(input$radio_indicator)) %>%
        top_n(n_highligth, wt = !!as.symbol(input$radio_indicator)) %>% .[,"Country.Region"] %>% as.vector()
      data = orig_data_aggregate %>% right_join(countries_order)  %>%  # reordering according to variable if istop
                mutate(Country.Region = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T]))
    } else {
      data = orig_data_aggregate

    }
    df_tmp <- data %>% .[,c("Country.Region", input$radio_indicator, "contagion_day")] %>%
      bind_cols(data[,input$radio_indicator] %>% setNames("Value")) %>%
      rename(Status = Country.Region ) %>%
      rename(Date = contagion_day ) %>%
      select(-input$radio_indicator)


    # Day of the country with max contagions after china
    max_contagion_no_china <- df_tmp %>%
      filter(Status != "China") %>%
      filter(Date == max(Date)) %>%
      select(Date) %>% unique() %>%
      as.numeric()
  df <- df_tmp %>%
      #filter(Status %in% as.vector(countries$Status)) %>% #pick only filtered countries, not needed, now done before
      filter(Date <= max_contagion_no_china) #%>% #cut china

    df
  })

  log <- reactive({
    input$radio_log_linear != "linear"
  })

  # Plot -----
  output$plot <- renderPlotly({
    p <- plot_all_highlight(df(), log = log(), text = "Area", n_highligth = n_highligth, percent = ifelse(input$radio_indicator == "lethality_rate", T, F), date_x = F, g_palette)
    p <- p %>%
      plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", itemsizing = "constant"))
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

