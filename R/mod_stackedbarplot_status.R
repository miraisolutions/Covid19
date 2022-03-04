#' stackedbarplot_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#'
#' @example man-roxygen/ex-mod_stackedbarplot_status.R
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
#' @noRd
mod_stackedbarplot_ui <- function(id){
  ns <- NS(id)
  caption_explain <- "The plot shows what areas have more to recover from their Confirmed cases. Not all of them may have provided Recoveries or Hospitalizations"

  fluidRow(
          div(htmlOutput(ns("title_stackedbarplot_status")), align = "center", class = "plottitle"),

          withSpinner(plotlyOutput(ns("plot_stackedbarplot_status"), height = 500)),
          div(caption_explain, align = "center", height = 10, class = "plottext")
      )
}
#' stackedbarplot_status Server Function
#'
#' @param df data.frame for multiple countries
#' @param w number of days of outbreak. Default 7
#' @param n_highlight number of countries considered.
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param statuses character vector of statuses in stacked barplot
#' @param active_hosp logical, if TRUE hosp and active are in status, active to be adjusted. Default FALSE
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import purrr
#' @importFrom plotly ggplotly layout
#' @noRd
mod_stackedbarplot_status_server <- function(input, output, session, df, w = 7, n_highlight = 5, istop = TRUE, statuses = c("deaths", "active", "recovered"), active_hosp = FALSE){
  ns <- session$ns

  # titles
  if (istop) {
    output$title_stackedbarplot_status <- renderText(#div(
      #h4(
        HTML(paste0("Current top ", n_highlight, " status split"))
      #, align = "center", style = "margin-top:20px; margin-bottom:20px;")
      )
  } else {
    output$title_stackedbarplot_status <- renderText(#div(
      #h4("Status split")
      HTML("Status split")
      #, align = "center", style = "margin-top:20px; margin-bottom:20px;")
      )
  }
  #active_hosp = FALSE
  if (active_hosp) {
    # if (sum(df$hosp, na.rm = TRUE)>0) {
    #   message("Using hospitalised data for stackedbarplot")
    #   statuses = append(statuses, "hosp", after = which(statuses == "deaths"))
    #   #active_hosp = TRUE
    # }
    message("Using hospitalised data for stackedbarplot")
    statuses = append(statuses, "hosp", after = which(statuses == "deaths"))
  }
  statuses_lab = names(varsNames(statuses))

  keepvars = c("Country.Region",statuses)
  if (istop)
    keepvars = c(keepvars, "confirmed")

  df_status = df %>%
    filter(date == AsOfDate) %>%
    select(Country.Region, !!keepvars)

  if (istop) {
    pick_status <- function(df, stat){
      df <-  df  %>%
        bind_cols(df[, stat] %>% setNames("Value"))
      df
    }
    df_status = pick_status(df_status, "confirmed") %>%
      #arrange(desc(Value)) %>%
      # top_n(n_highlight, wt = Value)  %>%
      slice_max(Value, n = n_highlight, with_ties = FALSE) %>%
      select(Country.Region,!!statuses) #%>% .[n_highlight:1, , drop = FALSE] # revert order to have largest on left
  }
  if (active_hosp) {
    df_status$active = pmax(replace_na(df_status$active,0) -  replace_na(df_status$hosp, 0), 0)
  }
  # add labels
  names(df_status)[names(df_status) %in% statuses] = statuses_lab # order is the same

  # gather status and compute ratios
  df_status_stack = df_status %>%
      gather("status", "countstatus", -Country.Region) %>%
    group_by(Country.Region) %>%
    mutate(n.pop = sum(countstatus, na.rm = TRUE),
           ratio.over.cases  = countstatus/n.pop) %>%
    ungroup() %>%
    # group_by(status) %>% # unused but could be useful
    # mutate(tot.status = sum(countstatus),
    #        ratio.status  = countstatus/tot.status) %>%
    # ungroup() %>%
    mutate(Country.Region = factor(Country.Region, levels = unique(df_status$Country.Region)),
           status = factor(status, levels = statuses_lab)) %>%
    arrange(status)

  #idx = order(match(df_status_stack$Country.Region, df$Country.Region))
  # df_status_stack = df_status_stack[idx, ]

  # output$caption <- renderText({
  #   caption_explain
  # })

  output$plot_stackedbarplot_status <- renderPlotly({
    p = stackedbarplot_plot(df_status_stack, percent = TRUE) %>% fix_colors(labs = TRUE)
    if (active_hosp) {
      # add back hospitalized and recompute
      p$data$countstatus[p$data$status == names(varsNames("active"))] =
        p$data$countstatus[p$data$status == names(varsNames("active"))] +  p$data$countstatus[p$data$status == names(varsNames("hosp"))]
      p$data = p$data %>% group_by(Country.Region) %>%
        mutate(n.pop = sum(countstatus, na.rm = TRUE),
               ratio.over.cases  = countstatus/n.pop*100) %>%
        ungroup()
    }
    p <- p %>%
      ggplotly(tooltip = c("text", "fill"))   %>%
      #layout(legend = list(orientation = "v", y = 1, yanchor = "left"))
      layout(legend = list(orientation = "h", y = 1.05, yanchor = "top", xanchor = "middle", x = 0))


    p$x$data <-
      p$x$data %>%
      purrr::map(clean_plotly_leg, "[^\\(][^,]*")

    p
  })
}
