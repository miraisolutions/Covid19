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
  vars = setdiff(names(case_colors), c("hosp","recovered")) # remove hosp for now
  choices_map <- c(vars, "new_confirmed","new_deaths", "new_active") %>%
    setNames(gsub("_", " ",c(vars, "new_confirmed", "new_deaths", "new_active"))) %>% as.list()
  div(
    #fluidPage(

    style = "position: relative;",
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map"), width = "100%", height = "800")),
    tags$head(tags$style(
      HTML('
             #input_date_control {background-color: rgba(192,192,192,0.6);;}
             #sel_date {background-color: rgba(0,0,255,1);}
             #help-block a {color: #ff0000 !important;}'
      )
    )),
    absolutePanel(
      #id = ns("input_date_control"), class = "panel panel-default",
      id = "input_date_control", class = "panel panel-default",
      width = "27.5vw",
      #height = "20vh",
      top = 10, left = 10, draggable = FALSE,
      #fixed = TRUE,
      div(style = "margin:10px;",
          uiOutput(ns("title_map")),
          radioButtons(inputId = ns("radio_choices"), label = "", choices = choices_map, selected = "confirmed", inline = T),
          radioButtons(inputId = ns("radio_pop"), label = "", choices = c("total", "per 1M pop"), selected = "total", inline = T),
          uiOutput(ns("slider_ui")),
          helpText("Click on the country to obtain its details."),
          div(uiOutput(ns("caption")), align = "center")

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

  #data <- orig_data_aggregate #%>% align_country_names()

  orig_data_aggregate$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(orig_data_aggregate$Country.Region, unique(as.character(countries_data_map$NAME)))])

  data_clean <- orig_data_aggregate %>%
    filter(!is.na(country_name))
  keepcols = c("country_name","Country.Region","date",
               names(data_clean)[sapply(data_clean, is.numeric)])
  data_clean = data_clean[, keepcols] # remove, not used

  # UI controls ----
  output$slider_ui <- renderUI({
    sliderInput(inputId = ns("slider_day"), label = "Day", min = min(orig_data_aggregate$date), max = max(orig_data_aggregate$date), value = max(orig_data_aggregate$date), dragRange = FALSE, animate = TRUE, step = 7)
  })

  # Map ----

  # Data for a given date
  data_date <- reactive({
    maxdate = req(input$slider_day)
    data_date <- data_clean %>%
      filter(date == maxdate) %>%
      filter(date == max(date)) %>%
      select(-c(Country.Region, date, contagion_day)) %>%
      group_by(country_name) %>%
      summarise_each(sum) %>%
      ungroup() %>%
      mutate(date = maxdate)
    data_date
  })

  data_plot <- reactive({
    data_selected <- data_date() %>%
      bind_cols(data_date()[,input$radio_choices] %>%
                  setNames("indicator"))

    if (req(input$radio_pop) == "per 1M pop") {
      max.pop = 100000
      data_selected <- data_selected %>%
        filter(population > max.pop) %>% # filter out very small countries
        # percentage of indicator per 1M population
        mutate(indicator = round(1000000 * .$indicator / .$population))
    }
    data_selected <- data_selected %>%
      select(country_name, indicator, update_ui()$textvar)

    data_plot <-  sp::merge(countries_data_map,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            sort = FALSE)
    # removed NAs can be shown
    #data_plot[["indicator"]] <- replace_na(data_plot[["indicator"]], 0)
    data_plot
  })
  update_ui <- reactive(update_radio(input$radio_choices, global = TRUE))

  if (isolate(!is.null(update_ui()$caption))) {
    output$caption <- renderUI(
      div(p(update_ui()$caption), align = "left",
          style = "margin-top:5px; margin-bottom:5px;")
    )
  }
  # add Title to output
  output$title_map <- renderUI(div(h4(update_ui()$graph_title), align = "center",
                                   style = "margin-top:10px; margin-bottom:0px;"))

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

    if(req(input$radio_pop) == "per 1M pop")
      var1M =   "per 1M pop"
    else {
      var1M = NULL
    }

    mapdata = leafletProxy("map", data = data_plot())  %>%
      addPolygons(layerId = ~NAME,
                  #fillColor = pal2()(dplyr::na_if(log(data_plot()$indicator), -Inf)),
                  fillColor = pal_fun(input$radio_choices, data_plot()$indicator)(pal_fun_calc(data_plot()$indicator, input$radio_choices)),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "mapdata",
                  label = ~NAME,
                  weight = 1,
                  popup = map_popup_data(data_plot(), "NAME", "indicator", input$radio_choices, update_ui()$textvar, namvarsfx = var1M),
                  popupOptions = popupOptions(keepInView = T, autoPan = F
                                              #autoPanPadding = c(100, 100)
                                              #offset = c(100,0)
                  )
                  #popup = country_popup()
      )
    mapdata =  addSearchFeatures(mapdata, targetGroups  = "mapdata",
                                 options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                                 position = "topright",hideMarkerOnCollapse = T,
                                                                 moveToLocation = FALSE))
    mapdata
  })
  # Add legend with new observe event
  observeEvent(data_plot(),{
    #mapdata
    leg_par <- legend_fun(data_plot()$indicator, input$radio_choices)
    proxy <- leafletProxy("map", data = countries_data_map)

    proxy = proxy %>% clearControls()
    do.call(what = "addLegend", args = c(list(map = proxy), leg_par, list(position = "bottomright")))

  })

}

# align_country_names <- function(data) {
#   # thanks to https://github.com/DrFabach/Corona/blob/master/shiny.r for data wrangling
#   # Note Cruise Ship and Mayonette not present in countries_data_map$NAME
#
#   data$Country.Region <- data$Country.Region %>%
#     recode(
#
#       "Macau" = "Macao",
#       "Mainland China" = "China",
#       "South Korea" = "South Korea",
#       "North Macedonia" = "Macedonia",
#       "Czech Republic" = "Czechia",
#       "Dominican Republic" = "Dominican Rep.",
#       "UK" = "United Kingdom",
#       "Gibraltar" = "United Kingdom",
#       # "US" = "United States",
#       "USA" = "United States",
#       "UAE" = "United Arab Emirates",
#       "Saint Barthelemy" = "St-Barth\\u00e9lemy", # stringi::stri_escape_unicode("é")
#
#       "Faroe Islands" = "Faeroe Is.",
#       "Bosnia and Herzegovina" = "Bosnia and Herz.",
#       "Vatican City" = "Vatican",
#       "Korea, South" = "South Korea",
#       "Republic of Ireland" = "Ireland",
#       "Taiwan*" = "Taiwan",
#       "St. Vincent Grenadines" = "St. Vin. and Gren.",
#       "Republic of the Congo" = "Dem. Rep. Congo", # corrected
#       "CAR" = "Central African Rep.",
#
#       "Congo (Kinshasa)" = "Congo",
#       "Cote d'Ivoire" = "C\\u00f4te d'Ivoire", # stringi::stri_escape_unicode("ô")
#       "Reunion" = "France",
#       "Martinique" = "France",
#       "French Guiana" = "France",
#       "Holy See" = "Vatican",
#       "Cayman Islands" = "Cayman Is.",
#       "Guadeloupe" = "France",
#       "Antigua and Barbuda" = "Antigua and Barb.",
#
#       "Curacao" = "Cura\\u00e7ao", # stringi::stri_escape_unicode("ç")
#       "Guadeloupe" = "France",
#       "occupied Palestinian territory" = "Palestine",
#       "Congo (Brazzaville)" = "Congo",
#       "Equatorial Guinea" = "Eq. Guinea", # corrected
#       "Central African Republic" = "Central African Rep.",
#       "Eswatini" = "eSwatini",
#
#       "Bahamas, The" = "Bahamas",
#       "Cape Verde" = "Cabo Verde",
#       "East Timor" = "Timor-Leste",
#       "Gambia, The" = "Gambia"
#
#
#     )
#   data
# }
