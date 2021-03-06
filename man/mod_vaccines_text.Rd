% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_vaccines_text.R
\name{mod_vaccines_text}
\alias{mod_vaccines_text}
\alias{mod_vaccines_text_ui}
\alias{mod_novaccines_text_server}
\alias{mod_vaccines_text_server}
\title{caseBoxes UI Function}
\usage{
mod_vaccines_text_ui(id)

mod_novaccines_text_server(input, output, session, country)

mod_vaccines_text_server(input, output, session, df, dftoday)
}
\arguments{
\item{id, }{Internal parameters for {shiny}.}

\item{df}{dataset whole time series.}

\item{dftoday}{dataset with today and weekly data.}
}
\description{
A shiny Module for displaying stats about vaccination
}
\examples{
if (interactive()) {
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_vaccines_text_ui("vaccines_text")
  )
  server <- function(input, output) {

    n <- 1000 #  min number of cases for a country to be considered. Default 1000
    w <- 7 # number of days of outbreak. Default 7

    # Data ----
    orig_data <- get_datahub(country = "Israel") \%>\%
      get_timeseries_by_contagion_day_data()


    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    # data_filtered <-
    #   orig_data_aggregate \%>\%
    #     Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    country_data <-
      orig_data_aggregate \%>\%
        #filter(Country.Region \%in\% "Switzerland") \%>\%
        filter(contagion_day > 0) \%>\%
        arrange(desc(date))

    lw_country_data =  lw_vars_calc(country_data)

    country_data_today = country_data \%>\%
      add_growth_death_rate()
    country_data_today = country_data_today  \%>\%
      left_join(lw_country_data \%>\% select(-population))

    callModule(mod_vaccines_text_server,"vaccines_text", dftoday = country_data_today, df = country_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

}
\keyword{internal}
