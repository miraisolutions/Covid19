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
                mod_plot_log_linear_ui(ns("plot_area_cont"))
         ),
     ),
    hr(),
    div(
      uiOutput(ns(paste("from_nth_case", uicont , sep = "_")))
    ),
    hr(),
    div(
      uiOutput(ns(paste("subcontinents_countries", uicont , sep = "_")))
    ),
    hr(),
    h2("Macro Area Comparison"),
    withSpinner(mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))),
    hr(),
    #withSpinner(uiOutput(ns(paste("lineplots_cont", uicont , sep = "_")))),
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
   hr(),
   div(h3("Country Plots within Continent"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
   hr(),
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("scatterplot_prev_countries", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("scatterplot_stringency_vars_countries", uicont , sep = "_"))))
     )
   ),
   hr(),
   div(h3("Country Heat Maps within Continent"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
   hr(),
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
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_tests_1M", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_positive_rate", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_stringency", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(uiOutput(ns(paste("map_countries_growthvsstringency", uicont , sep = "_"))))
     )
   ),
    mod_add_table_ui(ns(paste("add_table_cont", uicont , sep = "_"))),
    hr(),
    mod_add_table_ui(ns(paste("add_table_countries", uicont , sep = "_")))
    )
}

#' continent Server Function
#'
#' @param orig_data_aggregate data.frame with data from 1 continent
#' @param countries_data_map full sp data.frame with map details
#' @param nn min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param pop_data data.frame population
#' @param cont character continent or subcontinent name
#' @param uicont character continent or subcontinent name of ui
#' @import dplyr
#'
#' @noRd
mod_continent_server <- function(input, output, session, orig_data_aggregate, countries_data_map, nn = 1000, w = 7, pop_data, cont, uicont){
  ns <- session$ns

  message("Process continent ", cont)
  # statuses <- c("confirmed", "deaths", "recovered", "active")
  # # select all variables
  # allstatuses = c(statuses, paste0("new_", statuses), "tests", "hosp", "population")
  #allstatuses = get_aggrvars()

  orig_data_aggregate_cont <-
    orig_data_aggregate %>% filter(continent == cont)

 # subcontinents = reactive({sort(unique(orig_data_aggregate_cont$subcontinent))})
  subcontinents = sort(unique(orig_data_aggregate_cont$subcontinent))

  subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont)

  continent_data <-
    aggr_to_cont(orig_data_aggregate_cont, "continent", "date" )

  subcontinent_data <-
    aggr_to_cont(orig_data_aggregate_cont, "subcontinent", "date" )

  # define palette for subcontinent
  subcont_palette =
    palette_calc(col_cont = area_map_spec(cont, "col"),
         x = sort(unique(c(subcontinent_pop_data$subcontinent,
                           orig_data_aggregate_cont$subcontinent))))

  subcontinent_data_filtered <-
    subcontinent_data %>% # select sub-continents with longer outbreaks
      rescale_df_contagion(n = nn, w = w)

  subcontinent_data_filtered_today = subcontinent_data_filtered %>%
    add_growth_death_rate()

  lw_subcontinent_data_filtered =  lw_vars_calc(subcontinent_data_filtered)

  subcontinent_data_filtered_today = subcontinent_data_filtered_today  %>%
    left_join(lw_subcontinent_data_filtered %>% select(-population))


  continent_data_today <-
    continent_data %>%
      filter(date == max(date))

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
  callModule(mod_map_cont_server, paste("map_cont_ui", uicont , sep = "_"), orig_data_aggregate_cont, countries_data_map_cont, area = cont, g_palette = subcont_palette)

  # > area plot global
  levs <- areaplot_vars()

  df_continent =
    tsdata_areplot(continent_data,levs, nn = nn)

  callModule(mod_plot_log_linear_server, "plot_area_cont", df = df_continent, type = "area", g_palette = subcont_palette)
  output[[paste("from_nth_case", uicont , sep = "_")]]<- renderUI({
    HTML(paste(
      paste0(cont, " countries are grouped in Macro Areas as defined by United Nations."),
      paste0("The Areas are represented by the colors in the heatmap above, used also in the graphs of this page."),
      message_conf_case("Areas", nn, "are included"),
     # paste0("Only Areas with more than ", nn, " confirmed cases, and outbreaks longer than ", w, " days considered."),
      #paste0("Contagion day 0 is the first day with more than ", nn ," cases."),
      sep = "<br/>"))
  })
  # list of countries
  list.message =
      message_subcountries(orig_data_aggregate_cont,"subcontinent","Country.Region")

  output[[paste("subcontinents_countries", uicont , sep = "_")]]<- renderUI({
   HTML(paste(as.character(unlist(list.message)), collapse = '<br/>'))
  })

  # output[[paste("lineplots_cont", uicont , sep = "_")]] <- renderUI({
  #   tagList(
  #     h2("Macro Area Comparison"),
  #     mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))
  #   )
  # })

  callModule(mod_lineplots_day_contagion_server,
             "lineplots_day_contagion_cont",
             subcontinent_data, g_palette = subcont_palette, nn = nn)

  # Rate plots ----
  output[[paste("rateplots_cont", uicont , sep = "_")]] <- renderUI({
    mod_barplot_ui(ns("rate_plots_cont"))
  })

  callModule(mod_barplot_server, "rate_plots_cont", subcontinent_data_filtered_today,
             nn = nn, n_highligth = length(subcontinents), istop = FALSE,
             g_palette = list("plot_1" = subcont_palette,
                                       "plot_2" = subcont_palette,
                              calc = FALSE))

  # Line with bullet plot

  output[[paste("lines_points_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"), selectvar = "new_confirmed", hosp = FALSE, tests = FALSE, oneMpop = TRUE)
  })

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", subcontinent_data, nn = nn, w = w,
             n_highligth = length(subcontinents), istop = FALSE , g_palette = subcont_palette, hosp = FALSE, tests = FALSE, oneMpop = TRUE)

  # scatterplot
  output[[paste("scatterplot_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_cont"))
  })

  callModule(mod_scatterplot_server, "scatterplot_plots_cont",
             subcontinent_data_filtered_today, nmed = nn, n_highligth = length(subcontinents),
             istop = FALSE, countries = subcontinents)


  output[[paste("status_stackedbarplot_cont", uicont , sep = "_")]] <- renderUI({
    mod_stackedbarplot_ui(ns("status_stackedbarplot_cont"))
  })
  callModule(mod_stackedbarplot_status_server, "status_stackedbarplot_cont",
             subcontinent_data_filtered_today, n_highligth = length(subcontinents), istop = FALSE)

  # Compute Last week variables
  data7_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont)

  orig_data_aggregate_cont_today = orig_data_aggregate_cont %>%
    add_growth_death_rate()

  # scatterplot
  output[[paste("scatterplot_prev_countries", uicont , sep = "_")]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_prev_countries"))
  })

  # remove small countries
  countries200000 = sort(unique(orig_data_aggregate_cont_today$Country.Region[orig_data_aggregate_cont_today$population > 200000]))

  # create datasets for maps merging today with data7
  data_cont_maps = orig_data_aggregate_cont_today  %>%
    left_join(data7_aggregate_cont %>% select(-population))

  callModule(mod_scatterplot_server, "scatterplot_prev_countries",
             data_cont_maps, nmed = nn, n_highligth = length(countries200000),
             istop = FALSE, countries = countries200000)

  # output[[paste("scatterplot_stringency_countries", uicont , sep = "_")]] <- renderUI({
  #   mod_scatterplot_ui(ns("scatterplot_stringency_countries"))
  # })
  #
  # callModule(mod_scatterplot_server, "scatterplot_stringency_countries",
  #            orig_data_aggregate_cont_today, nmed = nn, n_highligth = length(countries200000),
  #            istop = FALSE, countries = countries200000, xvar = "stringency_index")

  output[[paste("scatterplot_stringency_vars_countries", uicont , sep = "_")]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_stringency_vars_countries"), growth = FALSE)
  })

  callModule(mod_scatterplot_server, "scatterplot_stringency_vars_countries",
             data_cont_maps, nmed = nn, n_highligth = length(countries200000),
             istop = FALSE, countries = countries200000, xvar = "stringency_index", growth = FALSE, fitted = FALSE)


  #maps confirmed
  output[[paste("map_countries_confirmed", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_confirmed"))
  })
  callModule(mod_map_area_calc_server, "map_countries_confirmed", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "confirmed")

  #maps active
  output[[paste("map_countries_active", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_active"))
  })
  callModule(mod_map_area_calc_server, "map_countries_active", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "active")

  #maps growth vs prev
  output[[paste("map_countries_growthvsprev", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growthvsprev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growthvsprev", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth vs prev")

  #maps prevalence
  output[[paste("map_countries_prev", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_prev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_prev", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "prevalence rate")
  #maps growth
  output[[paste("map_countries_growth", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growth"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growth", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth factor")

  #maps death
  output[[paste("map_countries_death", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_death"))
  })
  callModule(mod_map_area_calc_server, "map_countries_death", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "death")

  #maps tests
  output[[paste("map_countries_tests_1M", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_tests_1M"))
  })
  callModule(mod_map_area_calc_server, "map_countries_tests_1M", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "tests over 1M")

  #maps positive test rates
  output[[paste("map_countries_positive_rate", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_positive_rate"))
  })
  callModule(mod_map_area_calc_server, "map_countries_positive_rate", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "positive tests rate")

  #maps stringency index
  output[[paste("map_countries_stringency", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_stringency"))
  })
  callModule(mod_map_area_calc_server, "map_countries_stringency", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "stringency_index")

  #maps growth vs stringency index
  output[[paste("map_countries_growthvsstringency", uicont , sep = "_")]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growthvsstringency"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growthvsstringency", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth vs stringency")
  # tables ----
  callModule(mod_add_table_server, paste("add_table_cont", uicont , sep = "_"),
             subcontinent_data_filtered, maxrowsperpage = 10)

  # prepare data for table with country data
  orig_data_aggregate_cont_tab = orig_data_aggregate_cont %>% # only data from today
    filter(date == max(date)) %>%
    arrange(desc(confirmed) )
  callModule(mod_add_table_server, paste("add_table_countries", uicont , sep = "_"),
             orig_data_aggregate_cont_tab, maxrowsperpage = 10)

}

#' Derives palette for continent maps and graphs based on area_map_spec
#'
#' @param col_cont named character vector area_map_spec(cont, 'col')
#' @param x named character vector of areas
#' @return named character vector of colors for the areas
palette_calc = function(col_cont = area_map_spec(cont, "col"),  x ) {
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

  palcol = pal(x)
  names(palcol) = c(x) # add empty-s to remove first light colors

  palcol = palcol[!grepl("empty",names(palcol))]
  if (revpal) {
    names(palcol) = rev(names(palcol))
    palcol = rev(palcol)
  }

  palcol
}
