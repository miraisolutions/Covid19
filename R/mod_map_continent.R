#' Continent map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import leaflet.extras
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_cont_ui <- function(id){
  ns <- NS(id)
  div(
    #style = "position: absolute;",
    #tags$style(type = "text/css", "font: 'Arial'"),
    #tags$style(type = "text/css", "#map {width: calc(100vh - 80px) !important;}"),
    # tags$style(type = "text/css", ".child {position:relative;width:100%;height:auto}
    #               .parent {position:relative;width:100%;height:500}"),
    #style = "zoom: 0.8;", "html, body {width:100%;height:100%}
    tags$style(type = "text/css", " .leaflet .legend {font-size: 10px; line-height: 15px;font-family: 'Arial', sans-serif;}"),

    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    #withSpinner(leafletOutput(ns("map_cont"), width = "50vw", height = "500")) # the legend does not position correctly
    withSpinner(leafletOutput(ns("map_cont"), width = "100%",  height = "500")
                              )
  )
}

#' map Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#' @param countries_data_map data.frame sp for mapping
#' @param cont character continent or subcontinent name
#' @param g_palette character vector of colors for the graph and legend
#'
#' @example man-roxygen/ex-mod_map.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import sp
#'
#' @noRd
mod_map_cont_server <- function(input, output, session, orig_data_aggregate, countries_data_map, cont, g_palette){
  ns <- session$ns

  # Data ----

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
      mutate(country_name = factor(country_name, levels = sort(unique(country_name))))

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

  # pal_fun = function(dom, colpal){
  #   colorFactor(palette = colpal, domain = dom, na.color = "white")
  #
  # }

  output[["map_cont"]] <- renderLeaflet({
    # Using leaflet() to include non dynamic aspects of the map
    map = leaflet(
      data = data_plot(),
      options = leafletOptions(zoomControl = FALSE,
                               minZoom = cont_map_spec(cont, "zoom")*0.95, maxZoom = cont_map_spec(cont, "zoom")*1.05,
                               dragging = FALSE, # no need
                               centerFixed = TRUE,
                               maxBounds = list(
                                 c(cont_map_spec(cont, "lat")[1], cont_map_spec(cont, "lat")[2]),
                                 c(cont_map_spec(cont, "lat")[3], cont_map_spec(cont, "lat")[4])
                               ),
                               #browser.defaultWidth = "80%",
                               viewer.suppress = TRUE, knitr.figure = FALSE
       ))%>%
      setView(lng = mean(cont_map_spec(cont, "lat")[c(1,3)]), lat = mean(cont_map_spec(cont, "lat")[c(2,4)]),
              zoom = cont_map_spec(cont, "zoom")) %>%
      fitBounds(
        cont_map_spec(cont, "lat")[1], cont_map_spec(cont, "lat")[2],
        cont_map_spec(cont, "lat")[3], cont_map_spec(cont, "lat")[4]
      )%>%
      addPolygons(layerId = ~NAME,
                  # fillColor = pal_fun(factor(data_plot()[["indicator"]], levels = as.vector(sort(unique(data_plot()[["indicator"]])))),
                  #                     cont_map_spec(cont, "col"))(factor(data_plot()[["indicator"]], levels = as.vector(sort(unique(data_plot()[["indicator"]]))))),
                  fillColor = as.character(g_palette[data_plot()[["indicator"]]]),

                 fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "polygonsmap",
                  label = ~NAME,
                  weight = 1,
                  popup = country_popup(),
                  popupOptions = popupOptions(keepInView = T, autoPan = F
                                              #autoPanPadding = c(100, 100)
                                              #offset = c(100,0)
                    )
                  ) #%>%

      map =  addSearchFeatures(map, targetGroups  = "polygonsmap",
                               options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                               hideMarkerOnCollapse = T,
                                                               moveToLocation = FALSE)
                                )
      map = addLegend(map, position = cont_map_spec(cont, "legend"),
                #group = "legendmap",

                colors = as.character(g_palette), # [unique(data_plot()[["indicator"]])],
                opacity = 1,
                #labels = as.vector(sort(unique(data_plot()[["indicator"]]))),
                labels = names(g_palette),
                title = cont)#%>% #%>%
      map

   })

}
cont_map_spec <- function(cont, feat= c("lat","col","zoom")){

  lat = list(
          #"Europe" = c(32, 23, 72, 26) ,#lng1 lat1,lng2,lat2
          "Europe" = c(32, -5, 72, 40) ,#lng1 lat1,lng2,lat2
           "Africa" = c(-45, 0, 40, 36),
           "Asia" = c(0, 52, 47, 120),
           "Oceania" = c(-48, 110, 8, 180),
           "LatAm & Carib." =  c(-60, -80, 56, -55),
            "Northern America" = c(22, -147, 82, -39)
  )
  col = list("Europe" = "Blues", "Asia" = "Reds",
                          "Africa" = "RdYlBu", "Northern America" = "RdBu",
                          "LatAm & Carib." = "GnBu", "Oceania" = "Oranges")
  zoom = list("Europe" = 3,
                "Africa" = 2.9,
                "Asia" = 2.5,
                "Oceania" = 3.15,
                "LatAm & Carib." = 2.6,
                "Northern America" = 2.15
              )
  legend =  list("Europe" = "topright",
                 "Africa" = "bottomleft",
                 "Asia" = "bottomleft",
                 "Oceania" = "bottomleft",
                 "LatAm & Carib." = "bottomleft",
                 "Northern America" = "topright"
  )
  spec = list(lat = lat, col = col, zoom = zoom, legend = legend)
  spec[[feat]][[cont]]

}




