#' info text on timeline plots
#' @param sep separator, break line.
#' @param hosp logical, UCU message, default = TRUE.
#' @param country character, country text.
timeline_info <- function(sep = "<br/>", hosp = TRUE, country = "the world") {
  #tags$p(
  paste(paste0("Check how main variables developed over time for ",country,", 3 main type of plots are proposed: area plot for confirmed infections",
        ifelse(hosp, ", area plot of hospitalised an ICU admisions", ""), ", time line of each single variable from \"start\", \"6 months\" and \"1 month\" ago, calendar comparison for the \"full year\" or for the \"last month\".
        Change \"Select View\" to \"Timeline per calendar year\" to see the years' comparison."),
        sep = sep)
  #)
}

#' info text on vaccine doses
#' @param sep separator, break line.
vaccines_info <- function(sep = "<br/>") {
  #tags$p(
  paste("<b>Vaccines</b> are a simple count of injected doses, more detailed insight on full vaccination is not available in our data source yet, it will be added in the next releases. For this reason the vaccination over population size can have values > 100%.",
        "Areas that already started utilizing multiple doses (boosters) would result as more highliy vaccinated than others. It is reasonable to think that more doses mean higher vaccination, therefore vaccine doses can be put in reletion with other variables.",
        paste("Not all analyzed areas may have provided data on vaccines, our source is",
              a(
                href = "https://ourworldindata.org",
                target = "_blank",
                span(id = "vax-info-name", strsplit(caption_source_vaccines(), " ")[[1]][2]) # full mark present
              )),
        sep = sep)
  #)
}
#' info text on stringency index
#' @param sep separator, break line.
stringency_info <- function(sep = "<br/>") {
  #tags$p(
  paste(paste0("The <b>'",names(varsNames("stringency_index")),"'","</b> represents all the Lock Down measures with single index from 0 (no stringency) to 100 (full stringency)."),
        "This would include measures like school / work place closing, events cancellation, gathering restrictions, masks obligations, movements restrictions etc.",
        paste("More information about the construction of the 'stringency index' is available on the",
              a(
                href = "https://covid19datahub.io/articles/docs.html",
                target = "_blank",
                span(id = "str-info-name", "Covid-19 Data Hub")
              ), "documentation.", collapse = " "),
        sep = sep)
  #)
}
#' info text on confirmed cases
#' @param sep separator, break line.
confirmed_info <- function(sep = "<br/>") {
  #tags$p(
  paste("<b>Confirmed</b> infections are those resulting from a positive covid19 test that was communicated to the local authorities. From the infections other variables are being derived and used in this page:",
        "<b>Growth Factor</b>: total confirmed cases in the past 2 months as of today / total confirmed cases in the last 2 months 7 days ago.",
        "<b>Prevelence</b>: the impact of the infections on the population size. The prevalence is presented as cases over 1 million (1M) people, this view is utilised for many other additive variables.",
        "Other variables analysed in this section:",
        "<b>Mortality rate</b>: seen as the number of people who died of covid per 1 million (1M) people.",
        "<b>Lethality rate</b>: % of people who died of covid among those infected.",
        "Please consider that active cases may be unreliable for many countries that do not provide record of recovered cases, and that, where testing information is available, no differenciation is done between antigenic and molecolar test to derive the <b>Positive test rate</b>, i.e. % of positive tests. When comparing countries consider the different testing strategies and whether they report a comparable number of tests.", sep = sep)
  #)
}
#' info text on hospitalized
#' @param sep separator, break line.
hosp_info <- function(sep = "<br/>") {
  #tags$p(
  paste("The number of hospitalized patients ona a given date is provided by most of countries and regions within country, however please consider that hospitalized data could be missing also for some important countries.",
        paste(paste0("<b>'",names(varsNames("icuvent")), "'"), "</b> variable includes the number of hospitalized patients in intensive therapy and the patients requiring invasive ventilation on a date. The 2 measures have been merged as they are not presented with separated records by all countries", collapse = " "),
        sep = sep)
  #)
}

#' UI vaccination plots functions, scatter
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#' @param type character vaccines and stringency supported
#' @param infotext logical if TRUE then info text with results is printed
#'
#' @noRd
#'
#' @import shiny
mod_group_plot_ui <- function(id, type = c("vaccines", "stringency", "confirmed", "hosp"), infotext = TRUE){

  ns <- NS(id)
  growth = ifelse(type == "confirmed", TRUE, FALSE)
  hosp = ifelse(type == "hosp", TRUE, FALSE)


  section_info <- function(type, infotext) {
    if (infotext)
    switch(type,
           "confirmed" = confirmed_info(),
           "stringency" = stringency_info(),
           "vaccines" = vaccines_info(),
           "hosp" = hosp_info(),
           "")
    else
      ""
  }

  section_title <- function(type) {
    switch(type,
           "confirmed" = "Confirmed infections",
           "stringency" = "Stringency Lock-Down Index",
           "vaccines" = "Vaccination status",
           "hosp" = "Hospitalization",
           "")
  }
  hosp_vars <- function(type) {
    switch(type,
           "confirmed" = "remove",
           "stringency" = "keep",
           "vaccines" = "keep",
           "hosp" = "only",
           "keep")
  }

  fluidPage(
    hr(),
    div(section_title(type), align = "center", class = "sectiontitle"),
    br(),
    # conditionalPanel(
    #   condition = "infotext == 'TRUE'",
      fluidRow(
        column(12,
               div(
                 HTML(section_info(type, infotext)), class = "bodytext"),
                 htmlOutput(ns(paste0("text_report_", type))),

        )
      #)
    ),
    # br(),
    # fluidRow(
    #   column(6,
    #          div(
    #            HTML(scatterplot_info()), class = "bodytextplot")
    #   ),
    #   column(6,
    #          div(
    #            HTML(barplot_info()), class = "bodytextplot")
    #   )
    # ),
    br(),
    fluidRow(
      column(6,
             mod_scatterplot_ui(ns(paste0("scatterplot_", type)), growth = growth, hospvars = hosp_vars(type), text = TRUE)
             #)
      ),
      column(6,
             #withSpinner(
             mod_barplot_ui(ns(paste0("barplot_", type)), plot1 = paste0("ui_",type), plot2 = NULL, text = TRUE)
             #)
      )
    )
  )

}

#' Module for vaccination scatterplot and barplot
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param data_today data.frame with today variables
#' @param nn min number of cases for a country to be considered. Default n
#' @param w number of days of outbreak. Default 7
#' @param n_highlight number of countries to highlight if istop == TRUE
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param type character vaccines and stringency supported
#' @param scatterplotargs list(countries = NULL, nmed = 100)
#' @param barplotargs list(g_palette = list("plot_1" = barplots_colors[[type]]$calc, calc = TRUE),pickvariable = list("plot_1" = character(0), "plot_2" = character(0)))
#'
#' @import dplyr
#' @import shiny
#'
#' @seealso [mod_scatterplot_server], [mod_barplot_server]]
#' @noRd
mod_group_plot_server <-  function(input, output, session, data_today , nn = 1, w = 7, n_highlight = 10, istop = TRUE,
                                   type = c("vaccines", "stringency", "confirmed"),
                                   barplotargs = list(g_palette = list("plot_1" = barplots_colors[[type]]$calc, calc = TRUE),
                                                     pickvariable = list("plot_1" = character(0), "plot_2" = character(0)),
                                                     sortbyvar = TRUE),
                                   scatterplotargs = list(countries = NULL, nmed = 100)) {
  ns <- session$ns

  message(paste0("mod_",type,"_server"))

  countries = scatterplotargs[["countries"]]
  if (is.null(countries))
    countries = data_today %>%
      filter(confirmed > nn) %>%
      select(Country.Region) %>%
      distinct() %>% .$Country.Region

  nmed <- ifelse(is.null(scatterplotargs[["nmed"]]), 100 , scatterplotargs[["nmed"]])

  n_highlight <- ifelse(istop, n_highlight, length(countries))

  xvar <- switch(type,
                 "vaccines" = "vaccines_rate_pop",
                 "stringency" =  "stringency_index",
                 "confirmed" = "lm_confirmed_rate_1M_pop",
                 "hosp" = "lm_confirmed_rate_1M_pop" ,
                      stop("wrong type argument"))

  growth = ifelse(type == "confirmed", TRUE, FALSE)
  #lw_switch = ifelse(type == "stringency", FALSE, TRUE)

  report_var = switch(type,
                      "stringency" = "stringency_index",
                      "confirmed" = c("lw_confirmed_rate_1M_pop", "growth_factor_7", "lw_positive_tests_rate"),
                      "hosp" = paste0(prefix_var(.hosp_vars, c("","lw")), "_rate_1M_pop"),
                      "vaccines" = c(prefix_var("vaccines_rate_pop", c("","lw"))),
                      stop("wrong type argument"))
  if (all(is.na(data_today$population))) {
    # varsSelextX = list(label = div(style = "font-size:10px","Select (x) Total Confirmed Cases"),
    #                    choices = list("Over one week" = "lw_confirmed",
    #                                   #"Over 1 month" = "lm_confirmed",
    #                                   "Total" = "confirmed"),
    #                    selected = "confirmed")
    xvar = gsub("_rate_1M_pop$","", xvar)
    xvar = gsub("^lm_","", xvar)
    report_var = gsub("_rate_1M_pop$","", report_var)
  }

  text_lw_report_fun <- function(data, var, n = 5) {
    aod = max(data$AsOfDate)
    topN = lapply(var, function(x) {
      data[, c("Country.Region", x)] %>%
        slice_max(!!sym(x), n = n, with_ties = FALSE)  %>%
        select(Country.Region) %>%
        distinct() %>% .$Country.Region
    })
    names(topN) = var

    maxv =lapply(var, function(x){
      funformat(max(data[[x]], na.rm = TRUE), perc = ifelse(x %in% .rate_vars, TRUE, FALSE))
    })
    # maxv = funformat(max(data[[var]], na.rm = TRUE), perc = FALSE)
    maxC =lapply(var, function(x){
      data[["Country.Region"]][which.max(data[[x]])]
    })
    meanv =lapply(var, function(x){
      funformat(mean(data[[x]], na.rm = TRUE), perc = ifelse(x %in% .rate_vars, TRUE, FALSE))
    })
    names(meanv) = names(maxC) = names(maxv) = var

    # maxv = funformat(max(data[[var]], na.rm = TRUE), perc = FALSE)
    # maxC = data[["Country.Region"]][which.max(data[[var]])]
    # meanv = funformat(mean(data[[var]], na.rm = TRUE), perc = FALSE)
    msg0 = '<br/>'
    msg1 = NULL
    for (var.i in var) {
      msg1[[var.i]] = tags$li(strong(names(varsNames(var.i))), ': Average:',strong(meanv[[var.i]]),
                                     ', Maximum registered by', strong(paste("\"",maxC[[var.i]],"\"")), #'on', aod,
                                     ' as', strong(maxv[[var.i]]),
                            ". Top",n,"areas with the highest values (from left): ", paste(topN[[var.i]], collapse = '", "'))

    }
    # msg1 = tags$li(tags$p(strong('Maximum'), names(varsNames(var)), 'registered by', strong(maxC),'on', aod, ':', strong(maxv)))
    # msg2 = tags$li(tags$p(strong('Average'), names(varsNames(var)), 'registered on', aod, ':', strong(meanv)))
    # msg3 = tags$li(tags$p('Top 10 areas with the highest values (from left): ', paste(top10, collapse = ', '), ''))
    msg = tags$ul(msg1)
    #msg = c(msg0,msg)
    #msg = c(msg, "</ul>")
    #paste(msg, collapse = sep)
    div(tags$p("Records on", aod), msg, class = "bodytext")
  }
  output[[paste0("text_report_", type)]] <- #renderUI({
    renderUI({
        text_lw_report_fun(data_today, report_var)
  })

  callModule(mod_scatterplot_server, paste0("scatterplot_", type),
             data_today, nmed = nmed, n_highlight = n_highlight,
             istop = istop, countries = countries, xvar = xvar, growth = growth, fitted = FALSE)

  barplottitle <- ifelse(type == "vaccines", "Vaccine Doses",
                         ifelse(type == "stringency","Stringency Index",
                                ifelse(type == "confirmed", "Confirmed Cases",
                                       ifelse(type == "hosp", "Hospitalised / ICU Cases",
                                       stop("wrong type argument")))))

  sortbyvar = ifelse(is.null(barplotargs[["sortbyvar"]]), TRUE, barplotargs[["sortbyvar"]])
  pickvariable = switch(as.character(is.null(barplotargs[["pickvariable"]])),
                        "TRUE" = list("plot_1" = character(0), "plot_2" = character(0)),
                        "FALSE" = barplotargs[["pickvariable"]])
  g_palette = switch(as.character(is.null(barplotargs[["g_palette"]])),
                     "TRUE" = list("plot_1" = barplots_colors[[type]]$calc, calc = TRUE),
                     "FALSE"= barplotargs[["g_palette"]])
  callModule(mod_barplot_server, paste0("barplot_", type), data_today,
             n_highlight = n_highlight, istop = istop,
             plottitle = barplottitle,
             sortbyvar = sortbyvar,
             g_palette = g_palette,
             pickvariable = pickvariable)

}
