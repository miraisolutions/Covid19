#' bar_plot_day_contagion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bar_plot_day_contagion_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("bar_plot_day_contagion"))
  )
}

#' bar_plot_day_contagion Server Function
#'
#' @param country_data reactive data.frame for one country
#'
#' @importFrom dplyr select
#' @importFrom dplyr  mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#'
#' @noRd
mod_bar_plot_day_contagion_server <- function(input, output, session, country_data){
  ns <- session$ns

  statuses <- c("confirmed", "deaths", "recovered", "active")


  output$bar_plot_day_contagion <- renderPlot({
    df <- country_data() %>%
      select(-Country.Region, -date) %>%
      arrange(contagion_day)

    tmp <- sapply(statuses, function(s){
      df[,s] - df[, paste0("new_", s)]
    }) %>%
      setNames(
        paste0("diff_",statuses)
      ) %>%
      as.data.frame()

    df <- df %>%
      bind_cols(tmp) %>%
      pivot_longer(cols = -contagion_day, names_to = "status_all", values_to = "value") %>%
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
