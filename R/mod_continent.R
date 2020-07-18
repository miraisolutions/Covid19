#' continent_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param uicont character continent or region.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_continent_ui <- function(id, uicont){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns(paste("count-boxes", uicont , sep = "_"))),
     fluidRow(
       column(6,
            mod_map_cont_ui(ns(paste("map_cont_ui", uicont , sep = "_")))
       ),
      column(6,
            div(h4("Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                mod_plot_log_linear_ui(ns("plot_log_area_cont"))
         ),
     ),
    hr(),
    div(
      textOutput(ns(paste("from_nth_case", uicont , sep = "_")))
    ),
   hr(),
   div(
     uiOutput(ns(paste("subcontinents_countries", uicont , sep = "_")))
   ),
   hr(),
    withSpinner(uiOutput(ns(paste("lineplots_cont", uicont , sep = "_")))),
    fluidRow(
      column(5,
             withSpinner(uiOutput(ns(paste("rateplots_cont", uicont , sep = "_"))))
      ),
      column(7,
             withSpinner(uiOutput(ns(paste("lines_points_plots_cont", uicont , sep = "_"))))
             )
    ),
    fluidRow(
      column(6,
             withSpinner(uiOutput(ns(paste("scatterplot_plots_cont", uicont , sep = "_"))))
      ),
      column(6,
             withSpinner(uiOutput(ns(paste("status_stackedbarplot_cont", uicont , sep = "_"))))
      )
    ),
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_confirmed", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_active", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_growthvsprev", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_prev", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_growth", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_death", uicont , sep = "_"))))
     )
   ),
    mod_add_table_ui(ns(paste("add_table_cont", uicont , sep = "_")))
    )
}

#' continent Server Function
#'
#' @param orig_data_aggregate data.frame with data from 1 continent
#' @param countries_data_map full sp data.frame with map details
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param pop_data data.frame population
#' @param cont character continent or subcontinent name
#' @param uicont character continent or subcontinent name of ui
#' @import dplyr
#'
#' @noRd
mod_continent_server <- function(input, output, session, orig_data_aggregate, countries_data_map, n = 1000, w = 7, pop_data, cont, uicont){
  ns <- session$ns

  message("Process continent ", cont)
  # statuses <- c("confirmed", "deaths", "recovered", "active")
  # # select all variables
  # allstatuses = c(statuses, paste0("new_", statuses), "tests", "hosp", "population")
  #allstatuses = get_aggrvars()

  orig_data_aggregate_cont <-
    orig_data_aggregate %>% filter(continent == cont)

  data_filtered_cont <-
    orig_data_aggregate_cont %>% # select sub-continents with longer outbreaks
      rescale_df_contagion(n = n, w = w)


 # subcontinents = reactive({sort(unique(orig_data_aggregate_cont$subcontinent))})
  subcontinents = sort(unique(orig_data_aggregate_cont$subcontinent))


  subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) #%>%
    #group_by(subcontinent)# %>%
    #summarize(population = sum(population, na.rm = T))

  continent_data <-
    aggr_to_cont(orig_data_aggregate_cont, "continent", "date",
                                           #continent_pop_data,
                                           #allstatuses
                                           )

  subcontinent_data <-
    aggr_to_cont(orig_data_aggregate_cont, "subcontinent", "date",
                                              #subcontinent_pop_data,
                                              #allstatuses
                                              )
  # define palette for subcontinent

  subcont_palette =
    subcont_palette_calc(col_cont = cont_map_spec(cont, "col"),
         x = sort(unique(c(subcontinent_pop_data$subcontinent,
                           orig_data_aggregate_cont$subcontinent))))
 # })

  subcontinent_data_filtered <-
    subcontinent_data %>% # select sub-continents with longer outbreaks
      rescale_df_contagion(n = n, w = w)


  continent_data_today <-
    continent_data %>%
      filter(date == max(date))

  # continent_timeseries <-
  #   continent_data %>%
  #     get_timeseries_global_data()


  # filter map only continent
  #countries_data_map_cont = countries_data_map[countries_data_map@data$CONTINENT == cont,]

  .subsetmap = function(map,cc) {
    idx = map$CONTINENT %in% cc
    countries = map$NAME[idx]
    map_cont = subset(map, NAME %in% countries, drop = T)
    map_cont$CONTINENT = factor(map_cont$CONTINENT)
    map_cont$NAME = factor(map_cont$NAME)
    map_cont
  }
  countries_data_map_cont = .subsetmap(countries_data_map, cc = cont)

  # Boxes ----
  callModule(mod_caseBoxes_server, paste("count-boxes", uicont , sep = "_"), continent_data_today)

  # Map
  # Boxes ----
  callModule(mod_map_cont_server, paste("map_cont_ui", uicont , sep = "_"), orig_data_aggregate_cont, countries_data_map_cont, cont = cont, g_palette = subcont_palette)

  # > area plot global
  levs <- sort_type_hardcoded()

  df_continent =
    tsdata_areplot(continent_data,levs)

  callModule(mod_plot_log_linear_server, "plot_log_area_cont", df = df_continent, type = "area", g_palette = subcont_palette)

  output[[paste("from_nth_case", uicont , sep = "_")]]<- renderText({
    paste0("Only Areas with more than ", n, " confirmed cases, and outbreaks longer than ", w, " days considered. Contagion day 0 is the first day with more than ", n ," cases.")
  })
  # list of countries
  list.message = reactive({
      #message_subcountries(data_filtered_cont(),"subcontinent","Country.Region")
      message_subcountries(orig_data_aggregate_cont,"subcontinent","Country.Region")

  })

  output[[paste("subcontinents_countries", uicont , sep = "_")]]<- renderUI({
   HTML(paste(as.character(unlist(list.message())), collapse = '<br/>'))

  })

  output[[paste("lineplots_cont", uicont , sep = "_")]] <- renderUI({
    tagList(
      h2("Macro Area Comparison"),
      mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))
    )
  })

  callModule(mod_lineplots_day_contagion_server,
             "lineplots_day_contagion_cont",
             subcontinent_data_filtered, g_palette = subcont_palette)

  # Rate plots ----
  output[[paste("rateplots_cont", uicont , sep = "_")]] <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_cont"))
  })

  callModule(mod_growth_death_rate_server, "rate_plots_cont", subcontinent_data_filtered,
             n = n, n_highligth = length(subcontinents), istop = F, g_palette = list("growth_factor" = subcont_palette,
                                                                                       "death_rate" = subcont_palette))

  # Line with bullet plot

  output[[paste("lines_points_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"))
  })

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", subcontinent_data_filtered, n = n, w = w,
             n_highligth = length(subcontinents), istop = F , g_palette = subcont_palette)

  # scatterplot
  output[[paste("scatterplot_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_cont"))
  })

  callModule(mod_scatterplot_server, "scatterplot_plots_cont",
             subcontinent_data_filtered, n = n, n_highligth = length(subcontinents),
             istop = F, countries = subcontinents)


  output[[paste("status_stackedbarplot_cont", uicont , sep = "_")]] <- renderUI({
    mod_stackedbarplot_ui(ns("status_stackedbarplot_cont"))
  })
  callModule(mod_stackedbarplot_status_server, "status_stackedbarplot_cont",
             subcontinent_data_filtered, n = n, n_highligth = length(subcontinents), istop = F)

  #maps confirmed
  output[[paste("map_countries_confirmed", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_confirmed"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_confirmed", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "confirmed")

  #maps active
  output[[paste("map_countries_active", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_active"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_active", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "active")

  #maps growth vs prev
  output[[paste("map_countries_growthvsprev", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_growthvsprev"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_growthvsprev", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "growth vs prev")

  #maps prevalence
  output[[paste("map_countries_prev", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_prev"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_prev", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "prevalence rate")
  #maps growth
  output[[paste("map_countries_growth", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_growth"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_growth", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "growth factor")

  #maps death
  output[[paste("map_countries_death", uicont , sep = "_")]] <- renderUI({
    mod_map_cont_calc_ui(ns("map_countries_death"))
  })
  callModule(mod_map_cont_cal_server, "map_countries_death", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map_cont,
             cont = cont, variable = "death")

  # tables ----
  callModule(mod_add_table_server, paste("add_table_cont", uicont , sep = "_"),
             subcontinent_data_filtered, maxrowsperpage = 10)


}

#' Derives palette for continent maps and graphs based on cont_map_spec
#'
#' @param col_cont named character vector cont_map_spec(cont, 'col')
#' @param x named character vector of areas
#' @return named character vector of colors for the areas
subcont_palette_calc = function(col_cont = cont_map_spec(cont, "col"),  x ) {
  if (length(setdiff(names(col_cont), c("col","rev","skip"))) >0)
    stop("col_cont arg does not contain all names")

  skipn = as.integer(col_cont["skip"])
  revpal = as.logical(col_cont["rev"])
  if (skipn >0) { # assumption that first colors in palette are the lightes to be excluded
    emptycol = paste0("empty", 1:skipn)
    #if (!revpal) {
    x = c(x, emptycol)
    # }
    # else {
    #   x = c(emptycol, x)
    # }

  }
  pal = colorFactor(palette = col_cont["col"],
                    domain = x, na.color = "white",
                    ordered = TRUE,
                    reverse = TRUE)
  if (F) {
    #if using unikn
    pal = usecol(c(Karpfenblau, Seeblau), n = length(x))[-c(1)]
    pal = usecol(pal_karpfenblau, n = length(x))[-c(1)]
    names(pal) = x[-c(1)]
  }

  palcol = pal(x)
  names(palcol) = c(x) # add empty-s to remove first light colors

  palcol = palcol[!grepl("empty",names(palcol))]
  if (revpal) {
    names(palcol) = rev(names(palcol))
    palcol = rev(palcol)
  }

  palcol
}
