#' bar_plot_day_contagion UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_bar_plot_day_contagion_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("bar_plot_day_contagion"))
  )
}

#' bar_plot_day_contagion Server Function
#'
#' @param country_data data.frame for one country
#' @param datevar character variable used for X axis, date or contagion_day
#' @param nn minimum date derived from first day with more than nn cases. Default 1000
#' @param statuses variables to be used in barplot, 4  of .case.colors, default c("confirmed", "deaths", "recovered", "active")
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_bar_plot_day_contagion_server <- function(input, output, session, country_data, datevar = "date", nn = 1000, statuses = c("confirmed", "deaths", "recovered", "active")){
  ns <- session$ns

  # select all variables
  allstatuses = c(statuses, paste0("new_", statuses))

  output$bar_plot_day_contagion <- renderPlot({

    mindate = min(country_data$date[country_data$confirmed>nn], na.rm = TRUE)
    country_data = country_data %>% filter(date >= mindate)

    df <- country_data %>%
      ungroup() %>%
      #select(-Country.Region, -date) %>%
      select(!!datevar, !!allstatuses) %>%
      arrange(!!as.symbol(datevar)) #%>%
      #filter(confirmed > nn)

    tmp <- sapply(statuses, function(s){
      df[,s] - df[, paste0("new_", s)]
    }) %>%
      setNames(
        paste0("diff_",statuses)
      ) %>%
      as.data.frame()

    df <- df %>%
      bind_cols(tmp) %>%
      pivot_longer(cols = -all_of(datevar), names_to = "status_all", values_to = "value") %>%
      mutate(bool_new = case_when(
        grepl("new_", .$status_all) ~ "new",
        grepl("diff_", .$status_all) ~ "total",
        TRUE ~ "todrop"
      )) %>%
      filter(bool_new != "todrop") %>%
      mutate(bool_new = factor(bool_new, levels = c("total", "new"))) %>%
      mutate(status_all = gsub("diff_", "", .$status_all)) %>%
      mutate(status = factor(gsub("new_", "", .$status_all), levels = statuses)) %>%
      select(-status_all)

    df %>%
      from_contagion_day_bar_facet_plot()

  })

}
