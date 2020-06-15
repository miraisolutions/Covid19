### Covid19 1.1.2.9000 (development branch)
- correct plot title in country page (#94)
- Added continent split pages (#92)
  + added search box to global map
  + Merged Reunion Martinique French Guiana with France
  + replaced population data with UN dataset with 2020Q2 estimate
  + resized some texts in graphs

### Covid19 1.1.7 (2020-05-28)

- set different color -grey for NA in new active mod_map.R (#59)
- fix colour scale/legend over 1M by replacing data_clean() with data_plot() (#79)
- Country comp Lineplot from day of 1000 contagion rolling weekly average, remove dots, add imports zoo (#80)
- Fix colour order in mod_lineplots_day_contagion (#67)
- Merge the two boxes for current and new values into one.(#69)
- fix trend order of area plot both global and country tabs, added utils function (#74)
- correct plot titles for modules mod_compare_nth_cases_plot and mod_growth_death_rate (#66)
- Add angle to bar-plot labels (#68)
- Add scatterplot prevalence vs growth (#81)
- Add status split stackedbarplot (#83)

### Covid19 1.1.6 (2020-04-06)

- adding growth factor and lethality rate data to tables in country and country comparison tabs (#5)
- adding growth factor and lethality rate plots to country comparison tab (#5)
- adding comparison from 1000nd case to country comparison tab and country tab (#5)
- fix NA in input data
- add info to boxes #63
- remove pop up with data source information (#63)

### Covid19 1.1.5 (2020-04-06)

- fixed variables for map
- fixed albels for death rate plots

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
