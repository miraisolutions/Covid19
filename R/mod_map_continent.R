#' Continent map UI Function
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
mod_map_cont_ui <- function(id){
  ns <- NS(id)
  # choices_map <- c(names(case_colors), "new_confirmed", "new_active") %>%
  #   setNames(gsub("_", " ",c(names(case_colors), "new_confirmed", "new_active"))) %>% as.list()
  div(
    style = "position: relative;",
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map_cont"), width = "100%", height = "500")),
    # absolutePanel(
    #   id = ns("input_date_control_map_cont"), class = "panel panel-default",
    #   top = 10, left = 10, draggable = F#,
    #   # div(style = "margin:10px;",
    #   #     #radioButtons(inputId = ns("radio_choices"), label = "", choices = choices_map, selected = "confirmed", inline = T),
    #   #     #radioButtons(inputId = ns("radio_pop"), label = "", choices = c("total", "per 1M pop"), selected = "total", inline = T),
    #   #     #uiOutput(ns("slider_ui")),
    #   #     #helpText("Click on the country to obtain its details.")
    #   # )
    # )

  )
}

#' map Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#' @param countries_data_map data.frame sp for mapping
#' @param cont character continent or subcontinent name
#' @example man-roxygen/ex-mod_map.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#'
#' @noRd
mod_map_cont_server <- function(input, output, session, orig_data_aggregate, countries_data_map, cont, uicont){
  ns <- session$ns

  # Data ----
  #load data
  #countries_data_map <- load_countries_data(destpath = system.file("./countries_data", package = "Covid19"))

  # TODO could be replaced with other data and would be faster, change argument
  data_clean <- reactive({
    data <-
      orig_data_aggregate() %>%
      filter(continent == cont & date == max(date))  %>%# select data from continent only
      align_country_names() # align names with country data

    data$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(data$Country.Region, unique(as.character(countries_data_map$NAME)))])

    data_clean <- data %>%
      filter(!is.na(country_name))
    keepcols = c("country_name","Country.Region","continent", "subcontinent"
                 #names(data_clean)[sapply(data_clean, is.numeric)] # here the data, not needed
                 )
    data_clean = data_clean[, keepcols] # remove, not used
    data_clean
  })


  # Map ----

  data_plot <- reactive({
    data_selected <- data_clean() %>%
      bind_cols(data_clean()[,c("subcontinent")] %>%
                  setNames("indicator"))

    data_selected <- data_selected %>%
      select(country_name, indicator) %>%
      mutate((country_name = as.factor(country_name)))

    data_plot <-  sp::merge(countries_data_map,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            all.x = FALSE,
                            sort = FALSE)
    data_plot
  })

  # TODO: names should be restricted given boundaries
  country_popup <- reactive({
   paste0("<strong>Country: </strong>",
           data_plot()$NAME,
           "<br><strong>",
           "Area :",
           " </strong>",
           data_plot()[["indicator"]]
    )
  })

  pal_fun = function(dom,colpal){
    colorFactor(palette = colpal, domain = dom, na.color = "white")
  }

  output[["map_cont"]] <- renderLeaflet({
    # Using leaflet() to include non dynamic aspects of the map
    leaflet(
      data = data_plot(),
      options = leafletOptions(zoomControl = FALSE,
                               minZoom = cont_map_spec(cont, "zoom"), maxZoom = cont_map_spec(cont, "zoom"), dragging = FALSE,
                               maxBounds = list(
                                 c(cont_map_spec(cont, "lat")[1], cont_map_spec(cont, "lat")[2]),
                                 c(cont_map_spec(cont, "lat")[3], cont_map_spec(cont, "lat")[4])
                               ),
                               browser.defaultWidth = "80%",
                               viewer.suppress = TRUE, knitr.figure = FALSE
      ))  %>%

      # observeEvent(data_plot(),{
  # # update map with reactive part
    #leafletProxy("map_cont", data = data_plot(),
    # leaflet( data = data_plot() ,
    #          # options = leafletOptions(zoomControl = FALSE,
    #          #                          minZoom = 3, maxZoom = 3,
    #          #                          dragging = FALSE)
    # )  %>%
      addPolygons(layerId = ~NAME,
                  #fillColor = pal2(data_plot()$indicator),
                  fillColor = pal_fun(as.factor(data_plot()[["indicator"]]),
                                      cont_map_spec(cont, "col"))(as.factor(data_plot()[["indicator"]])),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "polygonsmap",
                  weight = 1,
                  popup = country_popup()) %>% # here boundaries get reset, fitBounds needed
            # addTiles(
            #    group = "tilesmap",
            #     ) %>%
                  #popup = pops) %>% # here boundaries get reset, fitBounds needed
      addLegend(position = "topright",
                #group = "legendmap",

                colors = pal_fun(as.factor(unique(data_plot()[["indicator"]])),
                                 cont_map_spec(cont, "col"))(as.factor(unique(data_plot()[["indicator"]]))),
                opacity = 1,
                labels = as.factor(unique(data_plot()[["indicator"]])),
                title = cont)#%>% #%>%
    #addLayersControl(
                #   baseGroups = c( "polygonsmap"),
                #   overlayGroups = c("tilesmap")
                # )
        #leaflet.extras::addFullscreenControl(pseudoFullscreen = T)
   })

}
cont_map_spec <- function(cont, feat= c("lat","col","zoom")){

  lat = list("Europe" = c(32, 23, 72, 26) ,
                       "Africa" = c(-40, 23, 40, 26),
                       "Asia" = c(13, 72, 34, 123),
                       "Oceania" = c(-45, 110, 5, 170),
                       "LatAm & Carib." =  c(-65, -80, 50, -55),
                        "Northern America" = c(20, -114, 85, -29)
  )
  col = list("Europe" = "Blues", "Asia" = "Reds",
                          "Africa" = "RdYlBu", "Northern America" = "RdBu",
                          "LatAm & Carib." = "GnBu", "Oceania" = "Oranges")
  zoom = list("Europe" = 3.4,
                "Africa" = 3,
                "Asia" = 2.9,
                "Oceania" = 3.4,
                "LatAm & Carib." = 2.6,
                "Northern America" = 2.3
              )
  spec = list(lat = lat, col = col, zoom = zoom)
  spec[[feat]][[cont]]

}




