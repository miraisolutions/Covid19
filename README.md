
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid19Mirai

<!-- badges: start -->

[![R build
status](https://github.com/miraisolutions/Covid19/workflows/CI-CD/badge.svg)](https://github.com/miraisolutions/Covid19/actions)
[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/vitalini-covid19?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/vitalini-covid19.html)
<!-- badges: end -->

The goal of Covid19Mirai is to fetch coronavirus data from public and
reliable resources available and analyze them.

## Data Source history

**Up to the 26th of March 2020**, the source was the [Johns Hopkins
University Center for Systems Science and Engineering (JHU
CSSE)](https://github.com/CSSEGISandData/COVID-19)

However, due to a format change on the data source, we had to swich,
**starting the 26th of March 2020**, to the
[work](https://github.com/bumbeishvili/covid19-daily-data) of
[bumbeishvili](https://github.com/bumbeishvili), who is maintaining the
old JHU CSSE data set format with updates declared as from
[worldometers](https://www.worldometers.info/coronavirus/). This is a
new project and we cannot guarantee the long-term reliability of the
data source. We are, however, grateful to
[bumbeishvili](https://github.com/bumbeishvili) for his
[work](https://github.com/bumbeishvili/covid19-daily-data).

**Update XX of June 2020**, Data from few European countries have been
readjusted recently, getting this update has given us the push to switch
to a new and richer data source. We have decided for the [COVID 19 Data
Hub](https://covid19datahub.io/) project lead by [Emanuele
Guidotti](https://guidotti.dev/) and [David
Ardia](https://ardiad.github.io/). We are very thankful to
[Bumbeishvili](https://github.com/bumbeishvili) for his great
[work](https://github.com/bumbeishvili/covid19-daily-data)

The results are visualized as a shiny app.

## Installation

You can install Covid19Mirai from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("miraisolutions/Covid19")
```
