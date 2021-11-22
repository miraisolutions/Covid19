#' mod_country_hosp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mod_country_hosp_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_hosp_area2"), type = "hosp", infotext = TRUE))
      ),
    ),
    hr(),
    fluidRow(

      column(6,
             withSpinner(uiOutput(ns("plot_compare_hosp_nth_area2")))

      ),
      column(6,
             div(h4("Time evolution of Hospitalised cases"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             #withSpinner(uiOutput(ns("plot_areahosp_area2")))
             withSpinner(mod_plot_log_linear_ui(ns("plot_areahosp2_area2"), select = TRUE, area = TRUE))

      )
    )


  )
}

#' mod_country_hosp Server Functions
#'
#' @noRd
mod_mod_country_hosp_server <- function(input, output, session, data_today, data, n2){
  ns <- session$ns
  # hosp session
  message("mod_mod_country_hosp_server")

  relevant_countries = unique(data_today$Country.Region[data_today$hosp > 0])

  callModule(mod_group_plot_server, "cmp_hosp_area2", data_today, type = "hosp", istop = FALSE,
             scatterplotargs = list(nmed = n2, countries = relevant_countries),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop")
             )
  ) # pi
  # Area plot
  # Area plot hospitalised ----
  levs <- areaplot_hospvars()

  #relevant_countries = unique(data$Country.Region[data$confirmed>1])


  df_area_2 = purrr::map(relevant_countries,
                         function(un) {
                           dat = tsdata_areplot(data[data$Country.Region == un, ], levs, nn = 1) #n = 0 for area plot hosp, do not filter
                           dat$Country.Region = rep(un, nrow(dat))
                           dat
                         })
  df_area_2 = Reduce("rbind",df_area_2)

  hospflag = sum(data$hosp, na.rm = TRUE) > 0
  oneMpopflag = TRUE
  if (all(is.na(data_today$population)))
    oneMpopflag = FALSE

  # some have no data here
  # output[["plot_areahosp_area2"]] <- renderUI({
  #   mod_plot_log_linear_ui(ns("plot_areahosp2_area2"), select = TRUE, area = TRUE)
  # })
  #if (hospflag)

  areas <- #reactive({
    df_area_2 %>%
    select(Country.Region) %>%
    distinct() #%>


  callModule(mod_plot_log_linear_server, "plot_areahosp2_area2", df = df_area_2, type = "area" , countries = reactive(areas), hosp = TRUE)

  output[["plot_compare_hosp_nth_area2"]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_plots_hosp_area2"), vars = intersect(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))),
                                  nn = n2, istop = FALSE, tests = FALSE, hosp = hospflag, strindx = FALSE,vax = FALSE, selectvar = "new_hosp", oneMpop = oneMpopflag, areasearch = TRUE)
  })
  callModule(mod_compare_nth_cases_plot_server, "lines_plots_hosp_area2", df = data, nn = n2,  istop = FALSE, tests = FALSE, hosp = TRUE, strindx = FALSE ,vax = FALSE,
                         n_highlight = length(unique(data$Country.Region)), oneMpop = oneMpopflag, areasearch = TRUE,
             vars = intersect(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))))

}

