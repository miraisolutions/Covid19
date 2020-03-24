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
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    leafletOutput(ns("map"), width = "100%", height = "800"),
    absolutePanel(id = ns("input_date_control"), class = "panel panel-default",
                  top = 350, left = 30, draggable = F,
                  div(style = "margin:10px;",
                      radioButtons(inputId = ns("radio_choices"), label = "", choices = c("confirmed", "deaths", "recovered", "active"), selected = "confirmed", inline = T),
                      uiOutput(ns("slider_ui")),
                      helpText("The detail of each country can be obtained by clicking on it.")
                  )
    )

  )
}

#' map Server Function
#'
#' @param data reactive data.frame
#'
#' @example man-roxygen/ex-mod_map.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#'
#' @noRd
mod_map_server <- function(input, output, session, data){
  ns <- session$ns

  # Data ----
  #load data
  countries_data <- load_countries_data(destpath = "./inst")

  data_clean <- reactive({
    data <- data()
    # align country names
    # thanks to https://github.com/DrFabach/Corona/blob/master/shiny.r for data wrangling
    data$Country.Region[data$Country.Region == "Macau"] <- "Macao"
    data$Country.Region[data$Country.Region == "Mainland China"] <- "China"
    data$Country.Region[data$Country.Region == "South Korea"] <- "South Korea"
    data$Country.Region[data$Country.Region == "North Macedonia"] <- "Macedonia"
    data$Country.Region[data$Country.Region == "Czech Republic"] <- "Czechia"
    data$Country.Region[data$Country.Region == "Dominican Republic"] <- "Dominican Rep."
    data$Country.Region[data$Country.Region == "UK"] <- "United Kingdom"
    data$Country.Region[data$Country.Region == "Gibraltar"] <- "United Kingdom"
    data$Country.Region[data$Country.Region == "US"] <- "United States"
    data$Country.Region[data$Country.Region == "Saint Barthelemy"] <- "St-Barthélemy"

    data$Country.Region[data$Country.Region == "Faroe Islands"] <- "Faeroe Is."
    data$Country.Region[data$Country.Region == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
    data$Country.Region[data$Country.Region == "Vatican City"] <- "Vatican"
    data$Country.Region[data$Country.Region == "Korea, South"] <- "South Korea"
    data$Country.Region[data$Country.Region == "Republic of Ireland"] <- "Ireland"
    data$Country.Region[data$Country.Region == "Taiwan*"] <- "Taiwan"

    data$Country.Region[data$Country.Region == "Congo (Kinshasa)"] <- "Congo"
    data$Country.Region[data$Country.Region == "Cote d'Ivoire"] <- "Côte d'Ivoire"
    data$Country.Region[data$Country.Region == "Reunion"] <- "France"
    data$Country.Region[data$Country.Region == "Martinique"] <- "France"
    data$Country.Region[data$Country.Region == "French Guiana"] <- "France"
    data$Country.Region[data$Country.Region == "Holy See"] <- "Vatican"
    data$Country.Region[data$Country.Region == "Cayman Islands"] <- "Cayman Is."
    data$Country.Region[data$Country.Region == "Guadeloupe"] <- "France"
    data$Country.Region[data$Country.Region == "Antigua and Barbuda"] <- "Antigua and Barb."

    data$Country.Region[data$Country.Region == "Curacao"] <- "Curaçao"
    data$Country.Region[data$Country.Region == "Guadeloupe"] <- "France"
    data$Country.Region[data$Country.Region == "occupied Palestinian territory"] <- "Palestine"
    data$Country.Region[data$Country.Region == "Congo (Brazzaville)"] <- "Congo"
    data$Country.Region[data$Country.Region == "Equatorial Guinea"] <- "Guinea"
    data$Country.Region[data$Country.Region == "Central African Republic"] <- "Central African Rep."
    data$Country.Region[data$Country.Region == "Eswatini"] <- "eSwatini"

    data$Country.Region[data$Country.Region == "Bahamas, The"] <- "Bahamas"
    data$Country.Region[data$Country.Region == "Cape Verde"] <- "Cabo Verde"
    data$Country.Region[data$Country.Region == "East Timor"] <- "Timor-Leste"
    data$Country.Region[data$Country.Region == "Gambia, The"] <- "Gambia"
    #Note Cruise Ship and Saint Vincent and the Grenadines not present in countries_data$NAME

    data$country_name <- as.character(unique(as.character(countries_data$NAME))[charmatch(data$Country.Region, unique(as.character(countries_data$NAME)))])

    data <- data %>%
      filter(!is.na(country_name))

    data
  })

  # UI controls ----
  output$slider_ui <- renderUI({
    sliderInput(inputId = ns("slider_day"), label = "Day", min = min(data()$date), max = max(data()$date), value = max(data()$date), dragRange = FALSE, animate = T, step = 1)
  })

  # Map ----

  # Data for a given date
  data_date <- reactive({
    data_clean() %>%
      filter(date == req(input$slider_day)) %>%
      select(-c(Country.Region, Province.State, Lat, Long, date, contagion_day)) %>%
      group_by(country_name) %>%
      summarise_each(sum)
  })

  data_plot <- reactive({
    data_selected <- data_date() %>%
      bind_cols(data_date()[,input$radio_choices] %>%
                  setNames("indicator")) %>%
      select(country_name, indicator)

    data_plot <-  sp::merge(countries_data,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            sort = FALSE)

   data_plot[["indicator"]] <- replace_na(data_plot[["indicator"]], 0)

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
    max(data_clean()[,input$radio_choices])
  })

  domain <- reactive({
    c(0,log(roundUp(max_value())))
  })

  pal2 <- reactive({
    # colorBin(palette = c("#FFFFFFFF",rev(viridis::inferno(256))), domain = c(0,roundUp(max_value())), na.color = "#f2f5f3", bins = 20)
    if (input$radio_choices == "confirmed") {
      colorNumeric(palette = "Reds", domain = domain(), na.color = "#f2f5f3")
    } else if (input$radio_choices == "deaths") {
      colorNumeric(palette = "Greys", domain = domain(), na.color = "#f2f5f3")
    } else if (input$radio_choices == "active") {
      colorNumeric(palette = "Blues", domain = domain(), na.color = "#f2f5f3")
    }  else if (input$radio_choices == "recovered") {
      colorNumeric(palette = "Greens", domain = domain(), na.color = "#f2f5f3")
    }
  })

  output$map <- renderLeaflet({
    # Using leaflet() to include non dynamic aspects of the map
    leaflet(data = countries_data) %>%
      setView(0, 30, zoom = 3)
  })

  # # update map with reactive part
  observeEvent(data_plot(),{
    leafletProxy("map", data = data_plot())  %>%
      addPolygons(layerId = ~NAME,
                  fillColor = pal2()(log(data_plot()$indicator)),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = country_popup())
  })

  observeEvent(data_plot(),{
    proxy <- leafletProxy("map", data = countries_data)
    proxy %>% clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal2(),
                opacity = 1,
                # values = data_plot()$indicator
                bins = log(10^(seq(2,log10(roundUp(max_value())),1))),
                values = log(1:roundUp(max_value())),
                data = log(1:roundUp(max_value())),
                labFormat = labelFormat(transform = function(x) roundUp(exp(x)) ,suffix = " cases")
      )
  })

}
