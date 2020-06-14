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
mod_map_cont_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_map")),
    uiOutput(ns("controls")), # radio buttons to be updated in server
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map_cont_calc"), width = "100%", height = "500")),
    div(uiOutput(ns("caption")), align = "center")

  )
}

#' map calc Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#' @param countries_data_map data.frame sp for mapping
#' @param cont character continent or subcontinent name
#' @param variable character variable name
#' @example man-roxygen/ex-mod_map_cont_calc.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import leaflet.extras
#'
#' @noRd
mod_map_cont_cal_server <- function(input, output, session, orig_data_aggregate, countries_data_map, cont, variable = "confirmed"){
  ns <- session$ns


  update_ui <- update_radio(variable)

  if (!is.null(update_ui$new_buttons)){
    # choices = reactive({update_ui$new_buttons$choices})
    # default = reactive({update_ui$new_buttons$selected})
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


  #ns <- session$ns

  # Data ----
  # if radio_time present  then evaluate, else take last day by default
  day = reactive({
    if (!is.null(req(new_var()))) {
      if(req(new_var()) == "last week")  7  else if (req(new_var())  == "today") 1 else 1
    }else
      1
    })


  data_clean <- reactive({
    data <- orig_data_aggregate() %>%
      filter(date %in% head(date,day()))  %>%# select data last 7 days or 1
              align_country_names()
    # if (grepl("(prevalence|rate)(?:.+)(prevalence|rate)", variable) ||
    #     grepl("death", variable)  ||
    #     grepl("(growth)*prev",variable)) {
    # remove for all variables, otherwise some countries appear in a map and not in another one
      message("remove very small countries not to mess up map")
      data = data %>%
        filter(population > 100000)
    # }
    data = data %>%
      mutate(growth_vs_prev = growth_v_prev_calc(data, growthvar = "growth_factor_3",prevvar = "prevalence_rate_1M_pop"))

    if (!is.null(button$radio) && req(button$radio) == "last week") {
      numvars = sapply(data, is.numeric)
      # remove factors?
      numvars = names(numvars)[numvars]
      data = data %>%
        group_by(Country.Region) %>%
          summarize_at(
            numvars , sum ) # average over the week
    }
    data$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(data$Country.Region, unique(as.character(countries_data_map$NAME)))])

    data_clean <- data %>%
      filter(!is.na(country_name))

    data_clean
  })


  # update variable name

  new_var_calc = function(var, but) {
    # if (!is.null(but) && !is.na(names(but$radio))) {
    #   newvar = ifelse(var == but$radio[[1]], var, but$radio[[1]])
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
                            all.x = FALSE,
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

  output[["map_cont_calc"]] <- renderLeaflet({
    map = leaflet(
      data = data_plot(),
      options = leafletOptions(zoomControl = FALSE,
                               minZoom = cont_map_spec(cont, "zoom"), maxZoom = cont_map_spec(cont, "zoom"),
                               dragging = TRUE,
                               centerFixed = TRUE,
                               maxBounds = list(
                                 c(cont_map_spec(cont, "lat")[1], cont_map_spec(cont, "lat")[2]),
                                 c(cont_map_spec(cont, "lat")[3], cont_map_spec(cont, "lat")[4])
                               ),
                               #sizingPolicy =leafletSizingPolicy(
                                 browser.defaultWidth = "80%",
                                 browser.fill = F,
                                 #browser.padding = 100,
                                 # viewer.suppress = TRUE, knitr.figure = FALSE,
                                 # knitr.defaultWidth = "100%"
                                 #)
      )) %>%
      setView(lng = mean(cont_map_spec(cont, "lat")[c(1,3)]), lat = mean(cont_map_spec(cont, "lat")[c(2,4)]),
              zoom = cont_map_spec(cont, "zoom"))
    leg_par <- legend_fun(data_plot()$indicator, new_var())
    map = map %>%
      addPolygons(layerId = ~NAME,
                  fillColor = pal_fun(new_var(), data_plot()$indicator)(pal_fun_calc(data_plot()$indicator)),
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
                                                     moveToLocation = FALSE)
                      )
      do.call(what = "addLegend", args = c(list(map = map), leg_par, list(position = cont_map_spec(cont, "legend"))))
      #%>% #%>%
    #addLayersControl(
    #   baseGroups = c( "polygonsmap,
    #   overlayGroups = c("tilesmap")
    # )
    #leaflet.extras::addFullscreenControl(pseudoFullscreen = T)
  })


}

varsNames = function(vars) {
  allvars = c(names(case_colors), paste("new", names(case_colors), sep = "_"),
              paste("growth_factor", c(3,5,7), sep = "_"),
              "lethality_rate", "mortality_rate_1M_pop",
              "prevalence_rate_1M_pop", "new_prevalence_rate_1M_pop", "population", "growth_vs_prev")
  allvars = allvars %>%
    setNames(gsub("_", " ", allvars))
  names(allvars)  = sapply(gsub("1M pop", "", names(allvars)), capitalize_first_letter)
  allvars = as.list(allvars)

  if (!missing(vars))
    res = allvars[unlist(allvars) %in% vars]
  else
    res = allvars
  res
}

update_radio<- function(var, growthvar = 3){

  graph_title = var
  textvar = NULL
  if (grepl("(growth)*fact",var)) { # growth factor
    new_buttons = list(name = "radio",
                       choices = varsNames()[grep("(growth)*fact", varsNames())], selected = varsNames("growth_factor_3"))
    caption <- paste0("growth factor: total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", growthvar) ," days ago.")
    graph_title = "Growth factor as of Today"

  } else if (grepl("(prevalence|rate)(?:.+)(prevalence|rate)",var)) {
    mapvar = grep("(prevalence|rate)(?:.+)(prevalence|rate)", varsNames(), value = T)
    #mapvar = varsNames()[mapvar]
    names(mapvar) = c("Total", "Today")
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["Today"])
    caption <- "Prevalence: confirmed cases over 1 M people."
    graph_title = "Prevalence of contagion over 1M"
    textvar = c("confirmed","new_confirmed","population")
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
    textvar = c("deaths", "new_deaths")

  } else if (grepl("(growth)*prev",var)) {
    new_buttons = NULL
    caption_growth_factor <- paste0("Growth Factor: total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", growthvar) ," days ago.")
    caption_prevalence <- "Prevalence: confirmed cases over 1 M people."
    caption =HTML(paste(c(caption_growth_factor,caption_prevalence), collapse = '<br/>'))
    graph_title = "Growth versus Prevalence"
    textvar = c("growth_factor_3", "prevalence_rate_1M_pop")
  } else if (grepl("active", var)) {
    mapvar = grep("active", varsNames(), value = T)
    #mapvar = varsNames()[mapvar]
    names(mapvar) = c("Total Active", "New Active Today")
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["New Active Today"])
    caption <- "Active cases today"
    graph_title = "Active cases today"

  } else {
    new_buttons = NULL
    caption = NULL
  }

  list(new_buttons = new_buttons, graph_title = graph_title, caption = caption, textvar= textvar)

}


update_var <- function(var, data, input){
  if (grepl("(growth)*fact",var)) { # growth factor
    new_var = input$radio
  } else if (grepl("death", var) || grepl("mortality", var)){ # growth factor
    new_var = ifelse(input$radio == "lethality rate", "lethality_rate", "mortality_rate_1M_pop")
  }else if (grepl("(prevalence|rate)(?:.+)(prevalence|rate)", var)) {
    new_var = ifelse(input$radio == "New", "new_prevalence_rate_1M_pop", "prevalence_rate_1M_pop")
  }else if (grepl("(growth)*prev",var)) {
    new_var = "growth_vs_prev"
  } else
    new_var = var

  list(new_var = new_var)

}

map_popup_data <- function(data, nam, ind, namvar, textvar){
  x = data[[ind]]
  NAME = data[[nam]]
  textvars = NULL
  varName = names(varsNames(namvar))
  if (!is.null(textvar)) {
    textvars = list(data = as.list(data@data[c(textvar)]),
                    NAME = names(varsNames(textvar)))
  }

  gen_text = function(x) {
    if (is.numeric(x)) {
      maxy = max(x)
      dg = nchar(as.character(round(maxy)))
      if(dg==1 && maxy<=1) {
        text.pop = paste0(roundlab(x*100),"%")
      } else {
        #text.pop = roundlab(x)
        text.pop = formatC(x, format = "f", big.mark = ",", digits  = getdg_lab(dg, maxy))
      }
    } else
      text.pop = x
    text.pop
  }

  text.pop.x = gen_text(x)
  paste_text = function(nam, txt, col = NULL) {
    if (!is.null(col)){
      col = c(paste0("<strong style='color:", col,";'>"))
    } else
      col = c("<strong>")

    paste0(
      #"<style> div.leaflet-popup-content {width:auto !important;}</style>",
           col, nam,":"," </strong>",
           txt,
           "<br>")
  }
  name_text = paste_text("Country", NAME)
  val_text = paste_text(varName, text.pop.x, "darkblue")


  text = paste0(name_text, val_text)

  if (!is.null(textvars)) {
    values.pop.vars = lapply(textvars$data,gen_text )
    names(values.pop.vars) = textvars$NAME
    tex.pop.vars =  lapply(names(values.pop.vars), function(nn)
      paste_text(nn, values.pop.vars[[nn]] ))
    # add text
    tex.pop.vars = c(list(text), tex.pop.vars)
    text = Reduce(paste0, tex.pop.vars)
  }
  text
}

legend_fun <- function(x, var){
  if (is.numeric(x)) { # if variable is numeric
    maxv = max(x)
    dg = nchar(as.character(round(maxv)))
    logmax = log(maxv)

    if (dg < 4){
      domain = choose_domain(x)
      bin = domain(x)
      bin = seq(bin[1],bin[2], length = 4)
      val = seq(min(bin),max(bin), length = 5000)
      dat = val
      suf = ""
      if (grepl("1M", var))
        suf = " over 1M"
      if (dg==1 && maxv<=1)
        suf = " %"
      transf = function(x,dg){
        if (dg==1 && maxv<=1)
          x = x * 100
        x
      }

      form = labelFormat(transform = function(x) x,
                         suffix = suf, digit = getdg_lab(dg, maxv))
    }  else { # high values, like total

      # TODO> simplify using domain()
      bound = max(log(round_up(-min(x))),log(round_up(maxv)))
      startbin = ifelse(any(x<0), -bound, 0)
      bin = seq(startbin, log(round_up(maxv)),
                length = 5)
      if(any(x<0))
        val = log(seq(1, exp(max(bin*2)), lenth = 10000)) -bound
      else
        val = log(1:exp(max(bin)))

      dat = val
      suf = ifelse(grepl("1M", var)," over 1M", " cases")
      form = labelFormat(transform = function(x) round_up(exp(x)), suffix = suf)
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
  res$opacity = 1
  res$pal = pal_fun(var, x)
  res
}
domainlog <- function(x) {
  maxv = max(x)
  c(0,log(round_up(maxv)))
}
domainlog_neg <- function(x) {
  maxv = max(x)
  bound = max(log(round_up(-min(x))),log(round_up(max(x))))
  c(-bound,bound)
}

domainlin <- function(x) {
  c(floor(min(x)),round_up(max(x)))
}
domainlin_neg <- function(x) {
  bound = max(round_up(-min(x)),round_up(max(x)))
  c(-bound,bound)
}
domainfact <- function(x) {
  #x = data_plot()$indicator
  unique(sort(x))
}
domainrate <- function(x) {
  c(floor(min(x)*100),round_up(max(x)*100))
}
choose_domain <- function(x) {
  if (is.numeric(x)) {
    maxy = max(x)
    dg = nchar(as.character(round(maxy)))

    if (dg == 1 && maxy <=1){ # if rate
      domain = domainrate
    } else if (dg <4) {
      if (any(x<0))
        domain = domainlin_neg
      else
        domain = domainlin
    } else {
      if (any(x<0))
        domain = domainlog_neg
      else
        domain = domainlog
    }
  } else
    domain = domainfact
  domain
}

pal_fun = function(var,x){
  domain = choose_domain(x)

  if (grepl("confirmed", var)  || grepl("(prevalence|rate)(?:.+)(prevalence|rate)", var)) {
    colorNumeric(palette = "Reds", domain = domain(x), na.color = "white")
  } else if (grepl("death", var) || grepl("mortality", var) || grepl("lethal", var)) {
    colorNumeric(palette = "Greys", domain = domain(x), na.color = "white")
  } else if (grepl("active", var)) {
    if (grepl("new", var)) {
      colorNumeric(palette = "PuOr", domain = domain(x), na.color = "grey")
    } else
      colorNumeric(palette = "Blues", domain = domain(x), na.color = "grey")

  }  else if (grepl("recovered", var)) {
    colorNumeric(palette = "Greens", domain = domain(x), na.color = "white")
  }  else if (grepl("(growth)*fact",var)) {
    colorNumeric(palette = "Oranges", domain = domain(x), na.color = "white")
  }  else if (grepl("(growth)*prev",var)) {
    colorFactor(palette = c("darkgreen", "#E69F00", "yellow3","#dd4b39"), domain = domain(x), ordered = TRUE, na.color = "white")
  }
  else
    stop("non existing color palette for ", var)
}

pal_fun_calc <- function(x){
  if (is.numeric(x)){
    maxv = max(x)
    dg = nchar(as.character(round(maxv)))
    if (dg == 1 && maxv<=1) {
      y = x *100 # rate
    } else if (dg <4) {
      # linear scale
      y = x
    } else {
      if (any(x<0)){
        y = log(x-min(x)+1) - log(-min(x))
      }
      else
        y = log(x)
    }
  } else
    y = x
  y
}
roundlab = function(y) {
  maxy = max(y)
  dg = nchar(as.character(round(maxy)))
  dglab = getdg_lab(dg, maxy)
  round(y, dglab)
}
