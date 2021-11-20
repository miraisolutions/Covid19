#' continent_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param uicont character continent or region.
#' @param nn min number of cases for used for mod_compare_timeline_plot_ui
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom stringr str_to_title
mod_continent_ui <- function(id, uicont, nn = 1000){

  contInfo = .getContinents()

  continent.name = stringr::str_to_title(contInfo$adjective[contInfo$ui == uicont])
  message("Continent in UI: ", continent.name)
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns(paste("count-boxes", uicont , sep = "_"))),
     fluidRow(
       column(6,
            mod_map_cont_ui(ns(paste("map_cont_ui", uicont , sep = "_")))
       ),
      column(6,
            #mod_plot_log_linear_ui(ns("plot_area_cont"))
            mod_compare_timeline_plot_ui(ns("plot_area_cont"), titles = 3:1,
                                         nn = nn, istop = FALSE,  hosp = FALSE, oneMpop = FALSE, vax = TRUE)
         ),
     ),
    hr(),
    div(
      HTML(paste(
        paste0(continent.name, " countries are grouped in Macro Areas as defined by United Nations."),
        paste0("The Areas are represented by the colors in the heatmap above, used also in the graphs of this page."),
        #message_conf_case("Areas", nn, "are included"),
        # paste0("Only Areas with more than ", nn, " confirmed cases, and outbreaks longer than ", w, " days considered."),
        #paste0("Contagion day 0 is the first day with more than ", nn ," cases."),
        sep = "<br/>")), class = "bodytext"),
    hr(),
    div(
      htmlOutput(ns(paste("ind_missing_days_countries", uicont , sep = "_"))), class = "bodytext"),
    hr(),
    div(
      htmlOutput(ns(paste("subcontinents_countries", uicont , sep = "_"))), class = "bodytext"),
    hr(),
    div(h4("Macro Area Comparison"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    withSpinner(mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))),
    hr(),
    fluidPage(
      # column(6,
      #        withSpinner(mod_scatterplot_ui(ns("scatterplot_plots_cont")))
      # ),
      # column(6,
      #        withSpinner(
      #          mod_barplot_ui(ns("barplot_vax_index_cont"), plot1 = "ui_vaccines", plot2 = NULL)
      #             )
      # )
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_withincont_confirmed"), type = "confirmed"))
      )
    ),
    hr(),
    fluidRow(
      column(5,
             #withSpinner(uiOutput(ns(paste("rateplots_cont", uicont , sep = "_"))))
             withSpinner(mod_barplot_ui(ns("rate_plots_cont")))

      ),
      column(7,
             withSpinner(#uiOutput(ns(paste("lines_points_plots_cont", uicont , sep = "_"))))
                mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"), selectvar = "new_confirmed", istop = FALSE, nn = 1000, hosp = FALSE, tests = FALSE, oneMpop = TRUE, vax = TRUE)
             )
      )
    ),
    hr(),
    fluidPage(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_withincont_vax"), type = "vaccines"))
      )
    ),
   hr(),
   div(h4("Country Plots within Continent"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
   hr(),
   column(12,
          withSpinner(mod_group_plot_ui(ns("cmp_confirmed_countries"), type = "confirmed", infotext = FALSE))
   ),
   hr(),
   column(12,
          withSpinner(mod_group_plot_ui(ns("cmp_hosp_countries"), type = "hosp", infotext = TRUE))
   ),
   hr(),
   column(12,
          withSpinner(mod_group_plot_ui(ns("cmp_stringency_countries"), type = "stringency", infotext = TRUE))
   ),
   # hr(),
   # fluidRow(
   #   column(12,
   #          withSpinner(mod_barplot_ui(ns("confirmed_hosp_plot_countries"), plot1 = "ui_confirmed", plot2  = "ui_hosp"))
   #   )
   # ),
   # hr(),
   # fluidRow(
   #   column(6,
   #          withSpinner(mod_scatterplot_ui(ns("scatterplot_prev_countries")))
   #   ),
   #   column(6,
   #          withSpinner(mod_scatterplot_ui(ns("scatterplot_stringency_vars_countries"), growth = FALSE))
   #   )
   # ),
   hr(),
   fluidPage(
   #   column(6,
   #          withSpinner(mod_barplot_ui(ns("barplot_vax_countries"), plot1 = "ui_vaccines", plot2  = NULL))
   #   ),
   #   column(6,
   #          withSpinner(mod_scatterplot_ui(ns("scatterplot_vax_vars_countries"), growth = FALSE))
   #   )
     column(12,
            withSpinner(mod_group_plot_ui(ns("cmp_cont_countries_vax"), type = "vaccines", infotext = FALSE))
     )
   ),
   hr(),
   div("Country Heat Maps within Continent", align = "center", class = "sectiontitle"),
   hr(),
   fluidRow(
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_confirmed")))
            #withSpinner(uiOutput(ns(paste("map_countries_confirmed", uicont , sep = "_"))))
     ),
     # column(6,
     #        withSpinner(mod_map_area_calc_ui(ns("map_countries_active")))
     #        #withSpinner(uiOutput(ns(paste("map_countries_active", uicont , sep = "_"))))
     # )
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_hosp")))
            #withSpinner(uiOutput(ns(paste("map_countries_active", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_growthvsprev")))
            #withSpinner(uiOutput(ns(paste("map_countries_growthvsprev", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_growth")))
            #withSpinner(uiOutput(ns(paste("map_countries_growth", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_vaccines")))
            #withSpinner(uiOutput(ns(paste("map_countries_vaccines", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_death")))
            #withSpinner(uiOutput(ns(paste("map_countries_death", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_tests_1M")))
            #withSpinner(uiOutput(ns(paste("map_countries_tests_1M", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_positive_rate")))
            #withSpinner(uiOutput(ns(paste("map_countries_positive_rate", uicont , sep = "_"))))
     )
   ),
   fluidRow(
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_stringency")))
            #withSpinner(uiOutput(ns(paste("map_countries_stringency", uicont , sep = "_"))))
     ),
     column(6,
            withSpinner(mod_map_area_calc_ui(ns("map_countries_growthvsstringency")))
            #withSpinner(uiOutput(ns(paste("map_countries_growthvsstringency", uicont , sep = "_"))))
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
  pw_subcontinent_data_filtered =  lw_vars_calc(subcontinent_data_filtered, 14)

  subcontinent_data_filtered_today = subcontinent_data_filtered_today  %>%
    left_join(lw_subcontinent_data_filtered %>% select(-population))  %>%
    left_join(pw_subcontinent_data_filtered %>% select(-population))


  continent_data_today <-
    continent_data %>%
      filter(date == AsOfDate)
  lw_continent_data_today =  lw_vars_calc(continent_data)
  pw_continent_data_today =  lw_vars_calc(continent_data, 14)

  continent_data_today = continent_data_today  %>%
    left_join(lw_continent_data_today %>% select(-population))  %>%
    left_join(pw_continent_data_today %>% select(-population))

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
  callModule(mod_caseBoxes_server, paste("count-boxes", uicont , sep = "_"), continent_data_today, vax = "recovered")

  # Map
  # Boxes ----
  callModule(mod_map_cont_server, paste("map_cont_ui", uicont , sep = "_"), orig_data_aggregate_cont, countries_data_map_cont, area = cont, g_palette = subcont_palette)

  # > area plot global
  levs <- areaplot_vars()
  #
  # df_continent =
  #   tsdata_areplot(continent_data,levs, nn = nn)

  callModule(mod_compare_timeline_plot_server, "plot_area_cont", continent_data, nn = nn, istop = FALSE,  hosp = FALSE, oneMpop = FALSE, vax = TRUE)

  #callModule(mod_plot_log_linear_server, "plot_area_cont", df = df_continent, type = "area", g_palette = subcont_palette)
  # output[[paste("from_nth_case", uicont , sep = "_")]]<- renderUI({
  #   HTML(paste(
  #     paste0(cont, " countries are grouped in Macro Areas as defined by United Nations."),
  #     paste0("The Areas are represented by the colors in the heatmap above, used also in the graphs of this page."),
  #     message_conf_case("Areas", nn, "are included"),
  #    # paste0("Only Areas with more than ", nn, " confirmed cases, and outbreaks longer than ", w, " days considered."),
  #     #paste0("Contagion day 0 is the first day with more than ", nn ," cases."),
  #     sep = "<br/>"))
  # })
  # list of countries
  list.message =
      message_subcountries(orig_data_aggregate_cont,"subcontinent","Country.Region")

  output[[paste("subcontinents_countries", uicont , sep = "_")]]<- renderText({
   HTML(
     paste(as.character(unlist(list.message)), collapse = '<br/>')
  )
  })


  callModule(mod_lineplots_day_contagion_server,
             "lineplots_day_contagion_cont",
             subcontinent_data, g_palette = subcont_palette, nn = nn, statuses = c("confirmed", "deaths", "vaccines", "active"))

  # Rate plots ----

  callModule(mod_barplot_server, "rate_plots_cont", subcontinent_data_filtered_today,
             n_highlight = length(subcontinents), istop = FALSE,
             g_palette = list("plot_1" = subcont_palette,
                                       "plot_2" = subcont_palette,
                              calc = FALSE),
             sortbyvar = FALSE)

  # Line with bullet plot

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", subcontinent_data, nn = nn,
             n_highlight = length(subcontinents), istop = FALSE , g_palette = subcont_palette, hosp = FALSE, tests = FALSE, oneMpop = TRUE, vax = TRUE)

  # scatterplot

  callModule(mod_group_plot_server, "cmp_withincont_confirmed", data_today = subcontinent_data_filtered_today, type = "confirmed", istop = FALSE,
             scatterplotargs = list(countries = subcontinents, nmed = nn),
             barplotargs = list(g_palette = list("plot_1" = subcont_palette, calc = FALSE), sortbyvar = FALSE))


  # callModule(mod_scatterplot_server, "scatterplot_plots_cont",
  #            subcontinent_data_filtered_today, nmed = nn, n_highlight = length(subcontinents),
  #            istop = FALSE, countries = subcontinents)
  #
  #
  # callModule(mod_barplot_server, "barplot_vax_index_cont", subcontinent_data_filtered_today,
  #            n_highlight = length(subcontinents), istop = FALSE,
  #            plottitle = c("Vaccination Status"),
  #            g_palette = list("plot_1" = subcont_palette,
  #                             calc = FALSE),
  #            sortbyvar = FALSE
  #            )
  callModule(mod_group_plot_server, "cmp_withincont_vax", data_today = subcontinent_data_filtered_today, type = "vaccines", istop = FALSE,
             scatterplotargs = list(countries = subcontinents, nmed = nn),
             barplotargs = list(g_palette = list("plot_1" = subcont_palette, calc = FALSE), sortbyvar = FALSE))


  # Compute Last week variables
  data7_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont)
  data14_aggregate_cont = lw_vars_calc(orig_data_aggregate_cont, 14)

  orig_data_aggregate_cont_today = orig_data_aggregate_cont %>%
    add_growth_death_rate()

  # scatterplot

  # remove small countries
  countries200000 = sort(unique(orig_data_aggregate_cont_today$Country.Region[orig_data_aggregate_cont_today$population > 200000]))

  # create datasets for maps merging today with data7
  data_cont_maps = orig_data_aggregate_cont_today  %>%
    left_join(data7_aggregate_cont %>% select(-population))  %>%
    left_join(data14_aggregate_cont %>% select(-population))

  # output[[paste("ind_missing_days_countries", uicont , sep = "_")]] <- renderUI({
  #   HTML(
  #     message_missing_country_days(data_cont_maps)
  #   )})
  output[[paste("ind_missing_days_countries", uicont , sep = "_")]] <- renderText({
   HTML(
             message_missing_country_days(data_cont_maps, sep = '<br/>'),
    )
    })

  callModule(mod_group_plot_server, "cmp_confirmed_countries", data_today = data_cont_maps, type = "confirmed", istop = FALSE,
             scatterplotargs = list(countries = countries200000, nmed = nn),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"))
      )

  callModule(mod_group_plot_server, "cmp_hosp_countries", data_today = data_cont_maps, type = "hosp", istop = FALSE,
             scatterplotargs = list(countries = countries200000, nmed = nn),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"))
  )

  callModule(mod_group_plot_server, "cmp_stringency_countries", data_today = data_cont_maps, type = "stringency", istop = FALSE,
             scatterplotargs = list(countries = countries200000, nmed = nn),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"))
  )
  # callModule(mod_barplot_server, "confirmed_hosp_plot_countries", data_cont_maps, #plottitle = c("Confirmed status"),
  #            istop = FALSE, n_highlight = length(countries200000),
  #            g_palette = list("plot_1" = barplots_colors$confirmed$uniform,
  #                             "plot_2" = barplots_colors$hosp$uniform,
  #                             calc = FALSE),
  #            pickvariable = list("plot_1" = "confirmed_rate_1M_pop","plot_2" = "hosp_rate_1M_pop"),
  #            plottitle =  c("Confirmed positive cases per Country", "Hospitalised and Intensive Care per Country")
  # )

  # callModule(mod_scatterplot_server, "scatterplot_prev_countries",
  #            data_cont_maps, nmed = nn, n_highlight = length(countries200000),
  #            istop = FALSE, countries = countries200000)
  #
  #
  # callModule(mod_scatterplot_server, "scatterplot_stringency_vars_countries",
  #            data_cont_maps, nmed = nn, n_highlight = length(countries200000),
  #            istop = FALSE, countries = countries200000, xvar = "stringency_index", growth = FALSE, fitted = FALSE)
  #

  # Rate plots ----
  # Vaccination

  # callModule(mod_barplot_server, "barplot_vax_countries", data_cont_maps,
  #            n_highlight = length(data_cont_maps$Country.Region), istop = FALSE,
  #            plottitle = c("Vaccination Status"),
  #            g_palette = list("plot_1" = barplots_colors[["vaccines"]]$calc,
  #                             calc = TRUE)#,
  #            #pickvariable = list("plot_1" = "confirmed_rate_1M_pop")
  #            )
  #
  #
  # callModule(mod_scatterplot_server, "scatterplot_vax_vars_countries",
  #            data_cont_maps, nmed = nn, n_highlight = length(countries200000),
  #            istop = FALSE, countries = countries200000, xvar = "vaccines_rate_pop", growth = FALSE, fitted = FALSE)
  callModule(mod_group_plot_server, "cmp_cont_countries_vax", data_today = data_cont_maps, type = "vaccines", istop = FALSE,
             scatterplotargs = list(countries = countries200000, nmed = nn),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"))
  )
  #############################################
  #maps confirmed
  callModule(mod_map_area_calc_server, "map_countries_confirmed", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "confirmed")

  #maps active
  # callModule(mod_map_area_calc_server, "map_countries_active", df = data_cont_maps,  countries_data_map_cont,
  #            area = cont, variable = "active")
  #maps active
  callModule(mod_map_area_calc_server, "map_countries_hosp", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "hospitalised")
  #maps growth vs prev
  callModule(mod_map_area_calc_server, "map_countries_growthvsprev", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth vs prev")

  #maps vaccines
  callModule(mod_map_area_calc_server, "map_countries_vaccines", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "vaccines")
  #maps growth
  callModule(mod_map_area_calc_server, "map_countries_growth", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth factor")

  #maps death
  callModule(mod_map_area_calc_server, "map_countries_death", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "death")

  #maps tests
  callModule(mod_map_area_calc_server, "map_countries_tests_1M", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "tests")

  #maps positive test rates
  callModule(mod_map_area_calc_server, "map_countries_positive_rate", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "positive tests rate")

  #maps stringency index
  callModule(mod_map_area_calc_server, "map_countries_stringency", df = data_cont_maps,  countries_data_map_cont,
            area = cont, variable = "stringency_index")

  #maps growth vs stringency index
  callModule(mod_map_area_calc_server, "map_countries_growthvsstringency", df = data_cont_maps,  countries_data_map_cont,
             area = cont, variable = "growth vs stringency")
  # tables ----
  callModule(mod_add_table_server, paste("add_table_cont", uicont , sep = "_"),
             subcontinent_data_filtered, maxrowsperpage = 10)

  # prepare data for table with country data
  orig_data_aggregate_cont_tab = orig_data_aggregate_cont %>% # only data from today
    filter(date == AsOfDate) %>%
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
