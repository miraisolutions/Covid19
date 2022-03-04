### Covid19Mirai 2.7.2-9000

### Covid19Mirai 2.7.1 (2022-02-28)
- Build data in package, updated with github action (#207)

### Covid19Mirai 2.6.1 (2021-11-22)
- Updated and replaced plots in multiple pages (#200)
- Added text info in pages
- Correction due to missing data in Canada

### Covid19Mirai 2.5.10 (2021-10-25)
- Fixed some small issues in labels (#197)

### Covid19Mirai 2.5.9 (2021-09-26)
- Improved lineplot, adding time frame and barplot option (#191)
- Axis labels shortened using K and M for thousands and millions
- Added button to open global map
- Added figures over bars in barplot

### Covid19Mirai 2.5.8 (2021-09-01)
- Improved speed UI and map (#188) 
- removed time slider map global
- Improved case boxes
- Adjusted weekly stats and hosp data

### Covid19Mirai 2.5.7 (2021-07-20)
- Added line-plot comparing calendar years (#185)
- Extended boxes with past week info

### Covid19Mirai 2.5.6 (2021-05-14)
- Updated data from covid19datahub, fetching raw data (#182)

### Covid19Mirai 2.5.5 (2021-03-23)
- Update vaccines text plot (#168)

### Covid19Mirai 2.5.4 (2021-03-19)
- Allow vaccination over population size to go beyond 1 (#168)

### Covid19Mirai 2.5.3 (2021-03-14)
- Review labels, renderUI, fix problem with USA level 2 (#175)

### Covid19Mirai 2.5.2 (2021-03-10)
- Remove shiny dashboard (#172)

### Covid19Mirai 2.5.1 (2021-03-10)
- Vaccination text plot, added navPage across main tabs (#168)

### Covid19Mirai 2.4.3 (2021-02-09)
- Correction scatterplot median lines for rates (0-1) (#163)

### Covid19Mirai 2.4.2 (2021-01-21)
- Correction to Last Week Vaccinated (#161)

### Covid19Mirai 2.4.1 (2021-01-19)
- Add vaccination maps and graphs (#157)

### Covid19Mirai 2.3.1 (2020-12-24)
- Add restriction policy index maps and graphs (#149)

### Covid19Mirai 2.2.4 (2020-11-24)
- Updated COVID19 package from Covid19datahub to 2.3.1 (#146)
https://covid19datahub.io/news/index.html
- Reviewed and used more consistently 1M pop rescaled vars in comparison (#134)

### Covid19Mirai 2.2.3 (2020-11-11)
- Review Hospitalised data (#133)

### Covid19Mirai 2.2.2 (2020-10-28)
- Fixed legend reactivity in global map (#135)

### Covid19Mirai 2.2.1 (2020-10-26)
- Removed contagion day in x axis, replaced with dates (#126)
- 2 maps added in continent page: tests per 1M, Positive test rates (#127)
- Enhanced global map in first page (#55)

### Covid19Mirai 2.2.0 (2020-09-04)
- Added Switzerland tab with maps at Kanton level (#20)
- Reviewed calculation and definition of Growth Factors (#122)
- Added new graph to Global page, selected top 10 in barplots

### Covid19Mirai 2.1.1 (2020-08-04)
- Added hospedalised area in area plot in country page where available (#114)
- Added some plots for level 2 areas within countries, if available (#115)
- Correction bug for country page China
- Updated UI for area plot, with level 2 selection, removed log option
- SelectInput instead of RadioButtons in rates plot
- Added Last Week option for Continent Maps

### Covid19Mirai 2.0.1 (2020-07-04)
- Fix broken deployment to shinyapps.io (#116)

### Covid19Mirai 2.0.0 (2020-07-03)
- Update data source from covid19datahub (#104)
https://github.com/covid19datahub/COVID19
- Population size taken from covid19datahub, some variables over 1M will differ
- Package name renamed to Covid19Mirai to avoid conflict with COVID19
- SP data saved in RDS for faster reading
- updated info on data source, updated ReadMe page
- correction on color palette for new active maps when there are no negative values

### Covid19 1.2.2 (2020-06-23)
- Fix colors in lineplot (#95)
- Align graph colors in continent map (#105)
- Updated graph style to align with new colors, updated palette
- Fixed error France, aggregation of colonies


### Covid19 1.2.1 (2020-06-15)
- correct plot title in country page (#94)
- Added continent split pages (#92)
  + added country maps withing continent
  + added search box to global map
  + Merged Reunion Martinique French Guiana with France
  + replaced population data with UN dataset with 2020Q2 estimate
  + resized some texts in graphs

### Covid19 1.1.7 (2020-05-28)

- Set different color -grey for NA in new active mod_map.R (#59)
- Fix colour scale/legend over 1M by replacing data_clean() with data_plot() (#79)
- Country comp Lineplot from day of 1000 contagion rolling weekly average, remove dots, add imports zoo (#80)
- Fix colour order in mod_lineplots_day_contagion (#67)
- Merge the two boxes for current and new values into one.(#69)
- Fix trend order of area plot both global and country tabs, added utils function (#74)
- correct plot titles for modules mod_compare_nth_cases_plot and mod_growth_death_rate (#66)
- Add angle to bar-plot labels (#68)
- Add scatterplot prevalence vs growth (#81)
- Add status split stackedbarplot (#83)

### Covid19 1.1.6 (2020-04-06)

- Adding growth factor and lethality rate data to tables in country and country comparison tabs (#5)
- Adding growth factor and lethality rate plots to country comparison tab (#5)
- Adding comparison from 1000nd case to country comparison tab and country tab (#5)
- Fix NA in input data
- Add info to boxes #63
- Remove pop up with data source information (#63)

### Covid19 1.1.5 (2020-04-06)

- Fixed variables for map
- Fixed labels for death rate plots

### Covid19 1.1.4 (2020-04-03)

- Added plot comparing countries from the day where the total number of confirmed cases was above 1000 (#39).
- Added growth factor and Death rate plots (#5)
- Added new cases to map (#56)

### Covid19 1.1.3 (2020-03-30)

- Integrate population data for map (#29)

### Covid19 1.1.2 (2020-03-26)

- JHU data source currently deprecated: switch (temporarily) to https://github.com/bumbeishvili/covid19-daily-data (#48). Users are alerted on app launch.

### Covid19 1.1.1 (2020-03-26)

- Updated JHU data source filenames.

### Covid19 1.1.0 (2020-03-25)

- Improved app header: it now shows the app version and the change-log in  a modal dialog (#38, #43).
- Added map showing the number of cases (#3, #36, #45).
- Added Loader animations to all plots (#40).

### Covid19 1.0.0 (2020-03-20)

#### First versioned release of Covid19

Covid19 is a Shiny App for understanding the COVID-19 spread and compare their perverse effects between countries.

The application uses data from the [Johns Hopkins CSSE Repository.](https://github.com/CSSEGISandData/COVID-19)
