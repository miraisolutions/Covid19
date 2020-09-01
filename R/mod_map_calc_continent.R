#' Continent map calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param variable character variable name
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_area_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type = "text/css", " .leaflet .legend {font-size: 10px; line-height: 15px;font-family: 'Arial', sans-serif;}"),

    uiOutput(ns("title_map")),
    uiOutput(ns("controls")), # radio buttons to be updated in server
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map_area_calc"), width = "100%", height = "500")),
    div(uiOutput(ns("caption")), align = "center")
)
 # )
}

#' map calc Server Function
#'
#' @param df data.frame as of today
#' @param countries_data_map data.frame sp for mapping
#' @param area character continent or subcontinent or area name
#' @param variable character variable name
#' @param max.pop integer cut off Country.Region with lower population
#'
#' @example man-roxygen/ex-mod_map_area_calc.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import leaflet.extras
#'
#' @noRd
mod_map_area_calc_server <- function(input, output, session, df, countries_data_map, area, variable = "confirmed", max.pop = 100000, countrymap = FALSE){
  ns <- session$ns

  update_ui <- update_radio(variable)
  if (!is.null(update_ui$new_buttons)){
    observe({

      output$controls <- renderUI({
        ns <- session$ns
        tagList(
          radioButtons(inputId = ns(update_ui$new_buttons$name), label = "",
                       choices = update_ui$new_buttons$choices,
                       selected = update_ui$new_buttons$choices[names(update_ui$new_buttons$selected)],
                       inline = T)
        )
      })

    })
    button = reactiveValues("radio" = update_ui$new_buttons$choices[[names(update_ui$new_buttons$selected)]])

    observe({
      req(input[[update_ui$new_buttons$name]])
      button[[update_ui$new_buttons$name]] =  input[[update_ui$new_buttons$name]]
    })
  } else
    button = NULL

  data_clean <- reactive({
    # data <- orig_data_aggregate %>%
    #   filter(date %in% head(date,day()))  #%>%# select data last 7 days or 1
     # remove for all variables, otherwise some countries appear in a map and not in another one
      message("remove very small countries not to mess up map")
      data = df %>%
        filter(population > max.pop)
    # TODO: it can be moved outside, in the data preparation
    if (grepl("(growth)*prev",variable))
      data = data %>%
        mutate(growth_vs_prev = growth_v_prev_calc(data, growthvar = "growth_factor_7",prevvar = "prevalence_rate_1M_pop"))

    data$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(data$Country.Region, unique(as.character(countries_data_map$NAME)))])

    data_clean <- data %>%
      filter(!is.na(country_name))

    data_clean
  })

  # update variable name

  new_var_calc = function(var, but) {
     if (!is.null(but) && !is.na(but$radio)) {
      newvar = ifelse(var == but$radio, var, but$radio)
    } else if (grepl("(growth)*prev",var)){
      newvar =  "growth_vs_prev"
    } else
      newvar = var
    message("new var = ",newvar)
    newvar
  }
  new_var =  reactive({
    new_var_calc(variable, button)
  })
  # Map ----
  data_plot <- reactive({
    data_selected <- data_clean() %>%
      bind_cols(data_clean()[,new_var()] %>%
                  setNames("indicator"))
    data_selected <- data_selected %>%
      select(country_name, indicator, update_ui$textvar) %>% # textvar contains additional columns for popup
      mutate(country_name = as.factor(country_name))

    if (!is.null(button$radio) && req(button$radio) == "per 1M pop") {
      data_selected <- data_selected %>%
        # percentage of indicator per 1M population
        mutate(indicator = round(1000000 * .$indicator / .$population))
    }

    data_plot <-  sp::merge(countries_data_map,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            all.x = TRUE, # set to T, it was F
                            sort = FALSE)
    data_plot
  })

  # add Title to output
  output$title_map <- renderUI(div(h4(update_ui$graph_title), align = "center",
                                   style = "margin-top:20px; margin-bottom:20px;"))

  # add caption info depending on plot
  if (!is.null(update_ui$caption)) {
    output$caption <- renderUI(
      p(update_ui$caption)
    )
  }

  output[["map_area_calc"]] <- renderLeaflet({
    if (!countrymap) {
      map = leaflet(
        data = data_plot(),
        options = leafletOptions(zoomControl = FALSE,
                                 minZoom = area_map_spec(area, "zoom")*0.95, maxZoom = area_map_spec(area, "zoom")*1.05,
                                 dragging = TRUE,
                                 centerFixed = TRUE,
                                 maxBounds = list(
                                   c(area_map_spec(area, "lat")[1], area_map_spec(area, "lat")[2]),
                                   c(area_map_spec(area, "lat")[3], area_map_spec(area, "lat")[4])
                                 ),
                                 #sizingPolicy =leafletSizingPolicy(
                                   browser.defaultWidth = "80%",
                                   browser.fill = F
                                   # browser.padding = 100,
                                   # viewer.suppress = TRUE, knitr.figure = FALSE,
                                   # knitr.defaultWidth = "100%"
                                   #)
        )) %>%
        setView(
          lng = mean(area_map_spec(area, "lat")[c(1,3)]), lat = mean(area_map_spec(area, "lat")[c(2,4)]),
                zoom = area_map_spec(area, "zoom"))
    } else {
      map = leaflet(
        data = data_plot(),
        options = leafletOptions(zoomControl = FALSE,
                                 minZoom = area_map_spec(area, "zoom")*0.975, maxZoom = area_map_spec(area, "zoom")*1.025,
                                 maxBounds = list(
                                   c(data_plot()@bbox["y","min"], data_plot()@bbox["x","min"]),
                                   c(data_plot()@bbox["y","max"], data_plot()@bbox["x","max"])
                                 ),
                                 dragging = TRUE,
                                 centerFixed = TRUE,
                                 #sizingPolicy =leafletSizingPolicy(
                                 browser.defaultWidth = "80%",
                                 browser.fill = F
        ))  %>% clearBounds() %>%
        setView(
          lng = mean(data_plot()@bbox["y",]), lat = mean(data_plot()@bbox["x",]),
          zoom = area_map_spec(area, "zoom"))
    }
    leg_par <- legend_fun(data_plot()$indicator, new_var())
    map = map %>%
      addPolygons(layerId = ~NAME,
                  fillColor = pal_fun(new_var(), data_plot()$indicator)(pal_fun_calc(data_plot()$indicator, new_var())),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "polygonsmap",
                  label = ~NAME,
                  weight = 1,
                  popup = map_popup_data(data_plot(), "NAME", "indicator", new_var(), update_ui$textvar),
                  popupOptions = popupOptions(keepInView = T, autoPan = F
                                              #autoPanPadding = c(100, 100)
                                              #offset = c(100,0)
                                              )

                  )
     map =  addSearchFeatures(map, targetGroups  = "polygonsmap",
                     options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                     hideMarkerOnCollapse = T,
                                                     moveToLocation = FALSE)
                      )
      do.call(what = "addLegend", args = c(list(map = map), leg_par, list(position = area_map_spec(area, "legend"))))

  })


}
#' List of variable names to be used for map
#' @param vars variable name to be selected, if empty tehn all are returned
#' @details The name of the list component correspond to the variable label
#' @return list All variables, if vars is missing, or one variable.
varsNames = function(vars) {
  allvars = c(names(case_colors), names(prefix_case_colors(prefix = "lw")),
              names(prefix_case_colors(prefix = "new")),
              paste("growth_factor", c(3,7,15), sep = "_"),
              "lethality_rate", "mortality_rate_1M_pop",
              "prevalence_rate_1M_pop", "lw_prevalence_rate_1M_pop", "new_prevalence_rate_1M_pop",
              "population", "growth_vs_prev",
              "tests","new_tests")
  allvars = allvars %>%
    setNames(gsub("_", " ", allvars))
  names(allvars)  = sapply(gsub("1M pop", "", names(allvars)), capitalize_first_letter)
  names(allvars)  = gsub("Lw", "Last Week", names(allvars))
  names(allvars)[grepl("hosp", allvars)] = gsub("Hosp", "Hospitalised", names(allvars)[grepl("hosp", allvars)])
  allvars = as.list(allvars)

  if (!missing(vars)){
    varnames = unlist(allvars)
    if (!all(vars %in% varnames))
      stop(paste(setdiff(vars, varnames), "invalid variable"))
    res = allvars[match(vars, varnames)]
  }
  else
    res = allvars
  res
}

#' Updates UI radiobuttons depending to variable va
#' @param var variable name
#' @param growthvar integer, 3 5 7 depending on choice

#' @return list list(new_buttons = new_buttons, graph_title = graph_title, caption = caption, textvar= textvar)
#' new_buttons: UI radiobuttons
#' graph_title: graph title
#' caption: vaption
#' textvar: variables to add in popup
update_radio<- function(var, growthvar = 7){

  graph_title = var
  textvar = NULL
  if (grepl("(growth)*fact",var)) { # growth factor
    new_buttons = list(name = "radio",
                       choices = varsNames()[grep("(growth)*fact", varsNames())], selected = varsNames(paste0("growth_factor_", growthvar)))
    #caption <- paste0("Growth Factor: total confirmed cases today / total confirmed cases (3 5 7) days ago.")
    caption_growth_factor <- caption_growth_factor_fun("(3 7 15)")

    graph_title = "Growth Factor"
    textvar = c("new_confirmed","lw_confirmed","confirmed","new_active")

  } else if (grepl("(prevalence|rate)(?:.+)(prevalence|rate)",var)) {
    mapvar = grep("(prevalence|rate)(?:.+)(prevalence|rate)", varsNames(), value = T)
    #mapvar = varsNames()[mapvar]
    namesmapvar = c("Total", "Last Week",
                    "Last Day")
    #TODO: order is not stable
    names(mapvar) = namesmapvar
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["Last Week"])
    caption <- "Prevalence: confirmed cases over 1 M people"
    graph_title = "Prevalence of contagion over 1M"
    textvar = c("new_confirmed","lw_confirmed","confirmed","population")
  } else if (grepl("death", var) || grepl("mortality", var)) {
    mapvar = c("Lethality Rate", "Mortality Rate")
    mapvar = varsNames()[mapvar]
    names(mapvar) = c("Lethality Rate", "Mortality over 1M pop")
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar[ "Mortality over 1M pop"])

    caption_leth_rate <- "Lethality Rate: total deaths today / total confirmed cases today"
    caption_mrt_rate <- "Mortality Rate: total deaths today per 1 M population"
    caption =HTML(paste(c(caption_leth_rate,caption_mrt_rate), collapse = '<br/>'))
    graph_title = "Death Rate"
    textvar = c("new_deaths", "lw_deaths", "deaths", "population")

  } else if (grepl("(growth)*prev",var)) {
    new_buttons = NULL
    #caption_growth_factor <- paste0("Growth Factor: total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", growthvar) ," days ago.")
    #caption_growth_factor <- paste0("Growth Factor: total confirmed cases since ", gsub("growth_factor_", "", growthvar)  ," days ago. / total confirmed cases in previous 30 days")
    caption_growth_factor <- caption_growth_factor_fun(growthvar)

    caption_prevalence <- "Prevalence: confirmed cases over 1 M people."
    caption =HTML(paste(c(caption_growth_factor,caption_prevalence), collapse = '<br/>'))
    graph_title = "Growth versus Prevalence"
    textvar = c("growth_factor_3", "new_prevalence_rate_1M_pop", "lw_prevalence_rate_1M_pop", "prevalence_rate_1M_pop")
  } else if (grepl("active", var)) {
    mapvar = grep("active", varsNames(), value = T)
    #mapvar = varsNames()[mapvar]
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["Last Week"])
    caption <- "Active values can be biased by non reported recovered cases"
    caption_color <- "Yellow scale to represent negative active."
    caption =HTML(paste(c(caption,caption_color), collapse = '<br/>'))

    graph_title = "Active cases"
    textvar = c("new_confirmed", "confirmed","new_recovered","recovered")
  } else if (grepl("confirmed", var)) {
    mapvar = grep("confirmed", varsNames(), value = T)
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["Last Week"])
    caption <- "Total, Last Week and New Confirmed cases"
    graph_title = "Confirmed cases"
    textvar = c("growth_factor_3", "active", "tests")
  } else {
    new_buttons = NULL
    caption = NULL
  }

  list(new_buttons = new_buttons, graph_title = graph_title, caption = caption, textvar= textvar)

}

#' Utility for popup message in map
#' @param data map data
#' @param nam character: component of country names from data, NAME
#' @param ind character: component of values from data, indicator
#' @param namvar character: vector, additional variable names
#' @param textvar character: vector, textt for the additional variables
#' @return vector pop up messages, html
map_popup_data <- function(data, nam, ind, namvar, textvar){
  x = data[[ind]]
  NAME = data[[nam]]
  textvars = NULL
  varName = names(varsNames(namvar))
  if (!is.null(textvar)) {
    textvars = list(data = as.list(data@data[c(textvar)]),
                    NAME = names(varsNames(textvar)))
    names(textvars$data) = textvar
  }

  text.pop.x = gen_text(x, namvar)
  .paste_text = function(nam, txt, col = NULL) {

    if (!is.null(col)){
      coltext = c(paste0("<style='color:", col,";'>"))
    } else
      coltext = NULL
    .pastecol = function(char = NULL,ptxt){
      if (!is.null(char)) {
        if (!is.null(ptxt))
          char = gsub(">"," ",char)
        ptxt = gsub("<","",ptxt)
      } #else
        #ptext = paste0(ptxt, "</style>")
      paste0(char, ptxt)
    }
    paste0(
      #"<style> div.leaflet-popup-content {width:auto !important;}</style>",
      .pastecol(char = "<strong>",ptxt = coltext ), nam,":",
      " </strong>",
      .pastecol(ptxt = coltext ),txt, ifelse(is.null(col), "", "</style>"),
      "<br>")

  }

  name_text = .paste_text("Country", NAME, case_colors["confirmed"])
  val_text = .paste_text(varName, text.pop.x, "darkblue")

  text = paste0(name_text, val_text)

  if (!is.null(textvars)) {

    values.pop.vars = NULL

    for (tvar in textvar) {
      values.pop.vars[[tvar]] = gen_text(textvars$data[[tvar]], tvar)
    }
    #values.pop.vars = lapply(textvars$data,gen_text, namvar =  textvar)
    names(values.pop.vars) = textvars$NAME
    tex.pop.vars =  lapply(names(values.pop.vars), function(nn)
      .paste_text(nn, values.pop.vars[[nn]] ))
    # add text
    tex.pop.vars = c(list(text), tex.pop.vars)
    text = Reduce(paste0, tex.pop.vars)
  }
  text
}
#' Utility legend building
#' @param x numeric vector of map data
#' @param var character: variable name
#' @return list legend parameters
legend_fun <- function(x, var){
  if (is.numeric(x)) { # if variable is numeric
    maxv = max(x, na.rm =T)
    minxv = min(x, na.rm =T)
    dg = nchar(as.character(round(max(abs(minxv),maxv))))
    #dg = nchar(as.character(round(maxv)))
    domain = choose_domain(x, var)

    if (dg < 5){
      bin = domain(x)
      bin = seq(bin[1],bin[2], length = 4)
      val = seq(min(bin),max(bin), length = 5000)
      dat = val
      suf = ""
      if (grepl("1M", var))
        suf = " over 1M"
      #if (dg==1 && maxv<=1 && minxv >= 0)
      if (var %in% rate_vars)
        suf = " %"
      transf = function(x,dg){
        #if (dg==1 && maxv<=1 && minxv >= 0)
        if (var %in% rate_vars)
          x = x * 100
        x
      }

      form = labelFormat(transform = function(x) x,
                         suffix = suf, digits = getdg_lab(dg, maxv, minxv))
    }  else { # high values, like total

      bin = domain(x)
      bin = seq(bin[1],bin[2], length = 5)
      if (F & any(x<0, na.rm = TRUE)) {
        #add 0, not possible
        bin = sort(c(bin, 0))
      }

      if(any(x<0, na.rm = TRUE))
        val = c(-log(1:exp(-min(bin))),log(1:exp(max(bin))))
      else
        val = log(1:exp(max(bin)))

      dat = val
      suf = ifelse(grepl("1M", var)," over 1M", " cases")
      .round_val = function(x){
       if (any(x<0, na.rm = TRUE)){
         y = rep(NA,length(x))
         y[!is.na(x) & x<0] = -round_up(exp(-x[!is.na(x) & x<0]))
         y[!is.na(x) & x>=0] = round_up(exp(x[!is.na(x) & x>=0]))
         y[is.infinite(y)] = 0 # perhaps to be moved also in the other case
       } else
           y = round_up(exp(x))
         y
      }
      form = labelFormat(transform = function(x) .round_val(x), suffix = suf)
    }
    res =     list(
      bins = bin,
      values = val,
      data = dat,
      labFormat = form)

  } else {
    label =  unique(sort(x))
    form = labelFormat(transform = function(x) unique(sort(x)))
    res =     list(
      labels = label,
      values = unique(sort(x)),
      labFormat = form)

  }
  #res$position = "bottomright"
  #res$className= "panel-panel-default legend"
  res$opacity = 1
  res$pal = pal_fun(var, x)
  res
}
domainlog <- function(x) {
  maxv = max(x, na.rm = TRUE)
  c(0,log(round_up(maxv)))
}
domainlog_neg <- function(x) {
  maxv = max(x, na.rm = TRUE)
  minv = min(x, na.rm = TRUE)
  #bound = c(-log(round_up(-minv)),log(round_up(maxv)))
  maxlimit = max(abs(x), na.rm = T)
  bound = c(-log(round_up(maxlimit)),log(round_up(maxlimit)))
  bound
}

domainlin <- function(x) {
  c(max(0,round_up(min(x, na.rm = TRUE), down = TRUE)),round_up(max(x, na.rm = TRUE)))
}
domainlin_neg <- function(x) {
  maxlimit = max(abs(x), na.rm = TRUE)
  c(-round_up(maxlimit),round_up(maxlimit))
}
domainfact <- function(x) {
  unique(sort(x))
}
domaingrowth <- function(x) {
  c(1,round_up(max(x, na.rm = TRUE)))
}
domainrate <- function(x) {
  c(floor(min(x, na.rm = TRUE)*100),round_up(max(x, na.rm = TRUE)*100))
}
#' Utility to choose domain for legend
#' @param x numeric vector of map data
#' @param var character vector of variable name
#' @return numeric vector of range
choose_domain <- function(x, var) {
  if (is.numeric(x)) {
    maxy = max(x, na.rm = T)
    minxy = min(x, na.rm = T)
    dg = nchar(as.character(round(max(abs(minxy),maxy))))
    if (var %in% rate_vars){ # if rate
      domain = domainrate
    } else if (grepl("(growth)*fact",var)){
      # growth factors variables
      domain = domaingrowth
    } else if (dg < 5) {
      if (var %in% neg_vars && any(x<0, na.rm = TRUE))
        domain = domainlin_neg
      else
        domain = domainlin
    } else {
      if (var %in% neg_vars && any(x<0, na.rm = TRUE))
        domain = domainlog_neg
      else
        domain = domainlog
    }
  } else
    domain = domainfact
  domain
}
#' Utility derive palette given datadisplay.brewer.all()

#' @param x numeric vector of map data
#' @param var character: variable name
#'
#' @importFrom grDevices colorRampPalette
#' @return palette
pal_fun = function(var,x){
  domain = choose_domain(x, var)

  if (grepl("confirmed", var)  || grepl("(prevalence|rate)(?:.+)(prevalence|rate)", var)) {
    colorNumeric(palette = "Reds", domain = domain(x), na.color = "lightgray")
  } else if (grepl("death", var) || grepl("mortality", var) || grepl("lethal", var)) {
    colorNumeric(palette = "Greys", domain = domain(x), na.color = "lightgray")
  } else if (grepl("active", var)) {
    # totale active to be excluded because of bad data that could be negative
    if ((grepl("new", var) || grepl("lw", var)) && any(x<0, na.rm = TRUE)) {
      # colorRampPalette to customize and mix 2 palettes
       colorNumeric(palette = colorRampPalette(c("yellow", "#3c8dbc"), interpolate = "linear" )(length(x)),
                   domain = domain(x), na.color = "lightgray")

      } else
      colorNumeric(palette = "Blues", domain = domain(x), na.color = "lightgray")

  }  else if (grepl("recovered", var)) {
    colorNumeric(palette = "Greens", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("(growth)*fact",var)) {
    colorNumeric(palette = "Oranges", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("(growth)*prev",var)) {
    colorFactor(palette = c("darkgreen", "#E69F00", "yellow3","#dd4b39"), domain = domain(x), ordered = TRUE, na.color = "lightgray")
  }
  else
    stop("non existing color palette for ", var)
}
#' Utility calculate colors given palette
#' @param x numeric vector of map data
#' @param var character vector of variable name
#' @return rescaled x
pal_fun_calc <- function(x, var){
  if (is.numeric(x)){
    # maxv = max(x)
    # dg = nchar(as.character(round(maxv)))
    maxv = max(x, na.rm = T)
    minxv = min(x, na.rm = T)
    dg = nchar(as.character(round(max(abs(minxv),maxv))))

    #if (dg == 1 && maxv<=1 && minxv>=0) {
    if (var %in% rate_vars) {
      y = x *100 # rate
    } else if (dg < 5) {
      # linear scale
      y = x
    } else {
      if (any(x<0, na.rm = TRUE)){
        y = rep(NA,length(x))
        y[!is.na(x) & x<0] = -log(-x[!is.na(x) & x<0])
        y[!is.na(x) & x>=0] = log(x[!is.na(x) & x>=0])
        #y[is.infinite(y)] = 0 # perhaps to be moved also in the other case
        #y = log(x-min(x)+1) - log(-min(x))
      }
      else{
        y = log(x)
      }
    }
    y[is.infinite(y)] = 0 # perhaps to be moved also in the other case

  } else
    y = x


  y
}
#' Utility round labels in domain and map
#' @param y numeric vector of map data
#' @return numeric rounded x value
roundlab = function(y) {
  # maxy = max(y)
  # dg = nchar(as.character(round(maxy)))
  #
  maxy = max(y, na.rm = T)
  minxy = min(y, na.rm = T)
  dg = nchar(as.character(round(max(abs(minxy),maxy))))

  dglab = getdg_lab(dg, maxy, minxy)
  round(y, dglab)
}
