
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid19Mirai

<!-- badges: start -->

[![R build
status](https://github.com/miraisolutions/Covid19/workflows/CI-CD/badge.svg)](https://github.com/miraisolutions/Covid19/actions)
[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/vitalini-covid19?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/vitalini-covid19.html)
<!-- badges: end -->

The goal of Covid19Mirai is to provide an insight on coronavirus data
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

## The dashboard

It consists of 4 main pages:

  - **Global**: Summarizing top 5 countries in the world in each
    variable.  
  - **Continents**: Summarizing world data per continent (defined
    according to UN), underneath subtabs with insight within continent:
      - **Europe**: European data split by macroareas with heatmaps per
        country.  
      - **Asia**: Asia data split by macroareas with heatmaps per
        country.  
      - **Africa**: African data split by macroareas with heatmaps per
        country.  
      - **Latin America & Carib.**: South and Central American with
        Caribbean Isles by macroareas with heatmaps per country.  
      - **Northern America**: Northern America (USA and Canada) data
        split by macroareas with heatmaps per country.  
  - **Country**: Single country report.  
      - If available regional graphs and data will be displayed.
  - **Country Comparison**: Comparison report between N countries from
    all over the world.

## The Variables

The Covid19datahub project can allow us to use the following variables:

  - *confirmed*: number of confirmed cases. Usually tested positive.  
  - *recovered*: number of healed or tested negative cases. Some
    countries have stopped reporting recovered cases.  
  - *deaths*: number of dead confirmed cases.  
  - *tests*: number of tests. Not available for all countries.  
  - *hosp*: number of hospitalised confirmed cases. Not available for
    all countries.  
  - *population*: population size.

The following variables are computed by the application:

  - *active*: number of active cases, usually tested positive.
    *confirmed* - *recovered* - *deaths*.  
  - *prevalence over 1M*: number of confirmed cases per 1 Million
    inhabitants.  
  - *growth factors (3 5 7)*: number of confirmed cases today divided by
    the number of confirmed cases 3 5 7 days ago.  
  - *lethality rate*: number of *deaths* divided by the number of
    *confirmed* cases.  
  - *mortality rate*: number of *deaths* divided by the *population*
    size.

The results are visualized as a shiny app.

## Installation

You can install Covid19Mirai from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("miraisolutions/Covid19")
```
