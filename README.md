
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid19

<!-- badges: start -->

[![R build
status](https://github.com/miraisolutions/Covid19/workflows/CI-CD/badge.svg)](https://github.com/miraisolutions/Covid19/actions)
<!-- badges: end -->

The goal of Covid19 is to fetch coronavirus data from public and
reliable resources available and analyze them.

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

The results are visualized as a shiny app.

## Installation

You can install Covid19 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("miraisolutions/Covid19")
```
