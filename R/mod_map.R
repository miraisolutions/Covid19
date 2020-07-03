#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_ui <- function(id){
  ns <- NS(id)
  choices_map <- c(names(case_colors), "new_confirmed", "new_active") %>%
    setNames(gsub("_", " ",c(names(case_colors), "new_confirmed", "new_active"))) %>% as.list()
  div(
    style = "position: relative;",
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map"), width = "100%", height = "800")),
    absolutePanel(
      id = ns("input_date_control"), class = "panel panel-default",
      top = 10, left = 10, draggable = F,
      div(style = "margin:10px;",
          radioButtons(inputId = ns("radio_choices"), label = "", choices = choices_map, selected = "confirmed", inline = T),
          radioButtons(inputId = ns("radio_pop"), label = "", choices = c("total", "per 1M pop"), selected = "total", inline = T),
          uiOutput(ns("slider_ui")),
          helpText("Click on the country to obtain its details.")
      )
    )

  )
}

#' map Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param countries_data_map data.frame sp for mapping
#' @example man-roxygen/ex-mod_map.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import leaflet.extras
#' @noRd
mod_map_server <- function(input, output, session, orig_data_aggregate, countries_data_map){
  ns <- session$ns

  # Data ----
  #load data
  #countries_data <- load_countries_data(destpath = system.file("./countries_data", package = "Covid19"))

  #data_clean <- reactive({
    data <- orig_data_aggregate #%>% align_country_names()

    orig_data_aggregate$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(orig_data_aggregate$Country.Region, unique(as.character(countries_data_map$NAME)))])

    data_clean <- orig_data_aggregate %>%
      filter(!is.na(country_name))
    keepcols = c("country_name","Country.Region","date",
                 names(data_clean)[sapply(data_clean, is.numeric)])
    data_clean = data_clean[, keepcols] # remove, not used
    data_clean
  #})

  # UI controls ----
  output$slider_ui <- renderUI({
    sliderInput(inputId = ns("slider_day"), label = "Day", min = min(orig_data_aggregate$date), max = max(orig_data_aggregate$date), value = max(orig_data_aggregate$date), dragRange = FALSE, animate = T, step = 1)
  })

  # Map ----

  # Data for a given date
  data_date <- reactive({
    data_date <- data_clean %>%
      filter(date == req(input$slider_day)) %>%
      #filter(date == max(date)) %>%
      select(-c(Country.Region, date, contagion_day)) %>%
      group_by(country_name) %>%
      summarise_each(sum) %>%
      ungroup()
    data_date
  })

  data_plot <- reactive({
    data_selected <- data_date() %>%
      bind_cols(data_date()[,input$radio_choices] %>%
                  setNames("indicator"))

    if (req(input$radio_pop) == "per 1M pop") {
      data_selected <- data_selected %>%
        # percentage of indicator per 1M population
        mutate(indicator = round(1000000 * .$indicator / .$population))
    }

    data_selected <- data_selected %>%
      select(country_name, indicator)

    data_plot <-  sp::merge(countries_data_map,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            sort = FALSE)

    data_plot[["indicator"]] <- replace_na(data_plot[["indicator"]], 0)
    #data_plot[["indicator"]] <- data_plot[["indicator"]] # could be removed but the code would have to be adjusted to handle NAs
    data_plot
  })

  country_popup <- reactive({
    paste0("<strong>Country: </strong>",
           data_plot()$NAME,
           "<br><strong>",
           "Value :",
           " </strong>",
           data_plot()[["indicator"]]
    )
  })

  max_value <- reactive({
    max(data_plot()[["indicator"]])
  })

  domain <- reactive({
    c(0,log(roundUp(max_value())))
  })

  pal2 <- reactive({
    # colorBin(palette = c("#FFFFFFFF",rev(viridis::inferno(256))), domain = c(0,roundUp(max_value())), na.color = "#f2f5f3", bins = 20)
    if (input$radio_choices == "confirmed" | input$radio_choices == "new_confirmed") {
      colorNumeric(palette = "Reds", domain = domain(), na.color = "white")
    } else if (input$radio_choices == "deaths" | input$radio_choices == "new_deaths") {
      colorNumeric(palette = "Greys", domain = domain(), na.color = "white")
    } else if (input$radio_choices == "active" | input$radio_choices == "new_active") {
      colorNumeric(palette = "Blues", domain = domain(), na.color = "grey")
    }  else if (input$radio_choices == "recovered" | input$radio_choices == "new_recovered") {
      colorNumeric(palette = "Greens", domain = domain(), na.color = "white")
    }
  })

  output$map <- renderLeaflet({
    # Using leaflet() to include non dynamic aspects of the map
    leaflet(
      data = countries_data_map,
      options = leafletOptions(zoomControl = FALSE) # not needed, clashes with slider
    ) %>%
      setView(0, 30, zoom = 3)
  })

  # # update map with reactive part
  observeEvent(data_plot(),{
    mapdata = leafletProxy("map", data = data_plot())  %>%
      addPolygons(layerId = ~NAME,
                  fillColor = pal2()(dplyr::na_if(log(data_plot()$indicator), -Inf)),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "mapdata",
                  label = ~NAME,
                  weight = 1,
                  popup = country_popup())
    mapdata =  addSearchFeatures(mapdata, targetGroups  = "mapdata",
                             options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                             position = "topright",hideMarkerOnCollapse = T,
                                                             moveToLocation = FALSE))
    mapdata

  })

  observeEvent(data_plot(),{
    proxy <- leafletProxy("map", data = countries_data_map)
    proxy %>% clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal2(),
                opacity = 1,
                # values = data_plot()$indicator
                bins = log(10^(seq(0,log10(roundUp(max_value())),1))),
                values = log(1:roundUp(max_value())),
                data = log(1:roundUp(max_value())),
                labFormat = labelFormat(transform = function(x) roundUp(exp(x)), suffix = paste0(" cases ", input$radio_pop))
      )
  })

}

align_country_names <- function(data) {
  # thanks to https://github.com/DrFabach/Corona/blob/master/shiny.r for data wrangling
  # Note Cruise Ship and Mayonette not present in countries_data_map$NAME

  data$Country.Region <- data$Country.Region %>%
    recode(

      "Macau" = "Macao",
      "Mainland China" = "China",
      "South Korea" = "South Korea",
      "North Macedonia" = "Macedonia",
      "Czech Republic" = "Czechia",
      "Dominican Republic" = "Dominican Rep.",
      "UK" = "United Kingdom",
      "Gibraltar" = "United Kingdom",
      # "US" = "United States",
      "USA" = "United States",
      "UAE" = "United Arab Emirates",
      "Saint Barthelemy" = "St-Barth\\u00e9lemy", # stringi::stri_escape_unicode("é")

      "Faroe Islands" = "Faeroe Is.",
      "Bosnia and Herzegovina" = "Bosnia and Herz.",
      "Vatican City" = "Vatican",
      "Korea, South" = "South Korea",
      "Republic of Ireland" = "Ireland",
      "Taiwan*" = "Taiwan",
      "St. Vincent Grenadines" = "St. Vin. and Gren.",
      "Republic of the Congo" = "Dem. Rep. Congo", # corrected
      "CAR" = "Central African Rep.",

      "Congo (Kinshasa)" = "Congo",
      "Cote d'Ivoire" = "C\\u00f4te d'Ivoire", # stringi::stri_escape_unicode("ô")
      "Reunion" = "France",
      "Martinique" = "France",
      "French Guiana" = "France",
      "Holy See" = "Vatican",
      "Cayman Islands" = "Cayman Is.",
      "Guadeloupe" = "France",
      "Antigua and Barbuda" = "Antigua and Barb.",

      "Curacao" = "Cura\\u00e7ao", # stringi::stri_escape_unicode("ç")
      "Guadeloupe" = "France",
      "occupied Palestinian territory" = "Palestine",
      "Congo (Brazzaville)" = "Congo",
      "Equatorial Guinea" = "Eq. Guinea", # corrected
      "Central African Republic" = "Central African Rep.",
      "Eswatini" = "eSwatini",

      "Bahamas, The" = "Bahamas",
      "Cape Verde" = "Cabo Verde",
      "East Timor" = "Timor-Leste",
      "Gambia, The" = "Gambia"


    )
  data
}
