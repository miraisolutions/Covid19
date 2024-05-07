
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid19Mirai

<!-- badges: start -->

[![ci-cd](https://github.com/miraisolutions/Covid19/actions/workflows/ci-cd.yml/badge.svg)](https://github.com/miraisolutions/Covid19/actions/workflows/ci-cd.yml)
[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/vitalini-covid19?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/vitalini-covid19.html)

<!-- badges: end -->

The goal of Covid19Mirai is to provide an insight on corona virus data
taken from public and reliable resources.

## Data Source history

**Up to the 26th of March 2020**, the source was the [Johns Hopkins
University Center for Systems Science and Engineering (JHU
CSSE)](https://github.com/CSSEGISandData/COVID-19)

However, due to a format change on the data source, we had to switch,
**starting the 26th of March 2020**, to the
[work](https://github.com/bumbeishvili/covid19-daily-data) of [David
Bumbeishvili](https://github.com/bumbeishvili), who is maintaining the
old JHU CSSE data set format with updates declared as from
[worldometers](https://www.worldometers.info/coronavirus/). This is a
new project and we cannot guarantee the long-term reliability of the
data source. We are, however, grateful to [David
Bumbeishvili](https://github.com/bumbeishvili) for his
[work](https://github.com/bumbeishvili/covid19-daily-data).

**Update 4th of July 2020**, Data from few European countries have been
readjusted recently, getting this update has given us the push to switch
to a new and richer data source. We have decided for the [COVID 19 Data
Hub](https://covid19datahub.io/) project led by [Emanuele
Guidotti](https://guidotti.dev/) and [David
Ardia](https://ardiad.github.io/). We are very thankful to [David
Bumbeishvili](https://github.com/bumbeishvili) for his great
[work](https://github.com/bumbeishvili/covid19-daily-data)

**Update October 2023**, the app stops updating data and is frozen to `2022-09-01`.


## Data Storage

Data are updated with a delay of 40h, i.e. at 4pm CEST the Last date is
updated taking the yesterday date. This allows having data for about all
countries when the date is updated with the new one.

The data are stored as RDS file in folder `inst/datahub` of the package.
A script `build_data` runs in **GitHub Actions** twice every day to update the data in the package.

The data update was stopped in October 2023.

## The dashboard

It consists of 5 main pages:

-   **Global**: Summarizing top 5 countries in the world in each
    variable.  
-   **Continents**: Summarizing world data per continent (defined
    according to UN), underneath sub-tabs with insight within continent:
    -   **Europe**: European data split by macro-areas with heat-maps
        per country.  
    -   **Asia**: Asia data split by macro-areas with heat-maps per
        country.  
    -   **Africa**: African data split by macro-areas with heat-maps per
        country.  
    -   **Latin America & Carib.**: South and Central American with
        Caribbean Isles by macro-areas with heat-maps per country.  
    -   **Northern America**: Northern America (USA and Canada) data
        split by macro-areas with heat-maps per country.  
-   **Switzerland**: Single country report of Switzerland.
    -   Maps and graphs of Cantonal data displayed.
-   **Country**: Single country report.
    -   If available regional graphs and data will be displayed.
-   **Country Comparison**: Comparison report between N countries from
    all over the world.

Macro areas of Continents are defined following United Nations
indications.

## The Variables

The Covid19datahub project can allow us to use the following variables:

-   *confirmed*: number of confirmed cases. Usually tested positive.  
-   *recovered*: number of healed or tested negative cases. Some
    countries have stopped reporting recovered cases.  
-   *deaths*: number of dead confirmed cases.  
-   *tests*: number of tests. Not available for all countries.  
-   *hosp*: number of currently hospitalised confirmed cases. Not
    available for all countries.  
-   *icuvent*: number of currently hospitalised Ventilated or in
    Intensive Care. Not available for all countries. Categorization
    differed from countries to country therefore Ventilated and
    Intensive Care variables have been aggregated.
-   *stringency index*: Lock Down stringency index from 0 to 100.  
-   *vaccines*: number of vaccine doses given to the population.  
-   *population*: population size.

The following variables are computed by the application:

-   *active*: number of active cases, usually tested positive.
    *confirmed* - *recovered* - *deaths*.  
-   *prevalence over 1M*: number of confirmed cases per 1 Million
    inhabitants.  
-   *growth factors (3 7 14)*: number of confirmed cases today divided
    by the number of confirmed cases 3 7 14 days ago over the previous 2
    months.
-   *lethality rate*: number of *deaths* divided by the number of
    *confirmed* cases.  
-   *mortality rate*: number of *deaths* divided by the *population*
    size.  
-   *positive test rate*: number of positive tests, i.e. *confirmed*
    divided by the *tests* carried in the day.  
-   *int care hospitalised rate*: number of patients currently in
    Intensive Care / Ventilated status, divided by the number of
    currently *hospitalised* patients.  
-   *vaccines rate*: number of *vaccines* divided by the *population*
    size.  
-   *new (variable)*: all variables labelled “new” are the delta of day
    X value - day X-1.  
-   *last week (variable)*: all variables labelled “last week” are the
    totals of the last 7 days.  
-   *past week (variable)*: all variables labelled “past week” are the
    totals of the previous 14 days to the last 7 days.  
-   *last month (variable)*: all variables labelled “last month” are the
    totals of the last 30 days.

The results are visualized as a shiny app.

## Installation

You can install Covid19Mirai from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("miraisolutions/Covid19")
```
