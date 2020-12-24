#' global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
mod_global_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("count-boxes")),
    fluidRow(
      mod_map_ui(ns("map_ui"))
    ),
    hr(),
    fluidRow(
      column(6,
             div(h4("Global Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_area_global"))
      ),
      column(6,
             withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_global"), hosp = FALSE, oneMpop = FALSE, tests = FALSE, actives = FALSE))
      )
    ),
    hr(),
    fluidRow(
      column(12,
             mod_barplot_ui(ns("plot_growth_death_rate"))
      )
    ),
    fluidRow(
      column(6,
             div(h4("Confirmed cases for top 5 countries"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_linear_top_n"), area = FALSE)
      ),
      column(6,
             mod_compare_nth_cases_plot_ui(ns("plot_compare_nth"), selectvar = "new_confirmed", hosp = FALSE, oneMpop = TRUE)
      )
    ),
    hr(),
    fluidRow(
      column(6,
             mod_scatterplot_ui(ns("plot_scatterplot_glob"))
             ),
      column(6,
             mod_stackedbarplot_ui(ns("plot_stackedbarplot_status"))
      )
    ),
    fluidRow(
      column(6,
             mod_scatterplot_ui(ns("plot_scatterplot_stringency_glob"), growth = FALSE)
      ),
      column(6,
             mod_barplot_ui(ns("plot_stringency_index"), plot1 = "ui_stringency", plot2 = NULL)
      )
    ),
    hr(),
    mod_add_table_ui(ns("add_table_world"))
  )
}

#' global Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param countries_data data.frame sp for mapping
#'
#' @import dplyr
#' @import tidyr
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#'
#' @noRd
mod_global_server <- function(input, output, session, orig_data_aggregate, countries_data_map){
  ns <- session$ns

  # Datasets ----

  # global_today <-
  #   orig_data %>%
  #     filter(date == max(date))

  # world time series
  # data at World level with get_timeseries_global_data
   total <-
     orig_data_aggregate %>%
     get_timeseries_global_data() %>% mutate(Country.Region = "World") %>%
     get_timeseries_by_contagion_day_data()


   total_aggregate <- total %>%  # add additional vars
     build_data_aggr()

   total_today <-
     total %>%
        filter(date == max(date))

   orig_data_aggregate = orig_data_aggregate %>%
     filter(population > 100000) # remove very small countries
  # countries today
  orig_data_aggregate_today <-
    orig_data_aggregate %>%
    add_growth_death_rate()

  lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

  orig_data_aggregate_today = orig_data_aggregate_today  %>%
    left_join(lw_orig_data_aggregate %>% select(-population))
  # TODO: REVIEW!
  world <-
    orig_data_aggregate_today %>%
      arrange(desc(confirmed) )
  # todp 5 countries today by confirmed
  world_top_5_today <-
    world %>%
      head(5)

  world_top_5_confirmed <-
    orig_data_aggregate %>%
      filter(Country.Region %in% world_top_5_today$Country.Region) %>%
      select(Country.Region, date, confirmed)

  # Boxes ----
  callModule(mod_caseBoxes_server, "count-boxes", total_today)

  # # map ----
  # data7_orig_data_aggregate = lw_vars_calc(orig_data_aggregate)
  #
  # # create datasets for maps merging today with data7
  # orig_data_aggregate_maps = orig_data_aggregate %>% filter(date == max(date)) %>%
  #   left_join(data7_orig_data_aggregate %>% select(-population))


  callModule(mod_map_server, "map_ui", orig_data_aggregate, countries_data_map)

  # plots ----
  levs <- areaplot_vars()

  n = 1000 # define areaplot start
  df_global =
    tsdata_areplot(total,levs, nn = n) # start from day with >1000

  callModule(mod_plot_log_linear_server, "plot_area_global", df = df_global, type = "area")

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_global", total_aggregate, nn = n, istop = FALSE,  hosp = FALSE, oneMpop = TRUE)

  # > line plot top 5
  #df_top_n <-
    # create factors with first top confirmed
  countries_order =  world_top_5_confirmed %>% filter(date == max(date)) %>%
    arrange(desc(confirmed)) %>%
     .[,"Country.Region"] %>% as.vector()

  # start lineplot from a later date
  mindate = min(world_top_5_confirmed$date[world_top_5_confirmed$confirmed>n], na.rm = TRUE)

  df_top_n = world_top_5_confirmed %>% filter(date > mindate) %>% # take only starting point where greater than 1000
    mutate(status = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T])) %>%
    mutate(value = confirmed) %>%
    capitalize_names_df()


  # lineplot of top 5 countries confirmed cases with date x axis
  callModule(mod_plot_log_linear_server, "plot_log_linear_top_n", df = df_top_n, type = "line")

  # > comparison plot from day of nth contagion


  callModule(mod_compare_nth_cases_plot_server, "plot_compare_nth", orig_data_aggregate, nn = n, hosp = FALSE, oneMpop = TRUE)

  # > growth_death_rate
  callModule(mod_barplot_server, "plot_growth_death_rate", orig_data_aggregate_today, n_highligth = 10)


  # > scatterplot prevalence vs growth, nmed = 10000 by default
  callModule(mod_scatterplot_server, "plot_scatterplot_glob", orig_data_aggregate_today, n_highligth = 10)
  # > scatterplot prevalence vs growth, nmed = 10000 by default
  callModule(mod_scatterplot_server, "plot_scatterplot_stringency_glob", orig_data_aggregate_today, n_highligth = 10, growth = FALSE, xvar = "stringency_index", istop = TRUE, fitted = FALSE)

  # > barplot stringency
  callModule(mod_barplot_server, "plot_stringency_index", orig_data_aggregate_today, n_highligth = 10, plottitle = c("Stringency Index"),
             g_palette = list("plot_1" = barplots_colors$stringency,
                              calc = TRUE),
             pickvariable = list("plot_1" = "confirmed")) # pick top 10 confirmed countries


  # > stacked barplot with status split
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status", orig_data_aggregate_today, n_highligth = 10, istop = TRUE)

  # tables ----
  callModule(mod_add_table_server, "add_table_world", world)


}


