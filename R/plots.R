#' stacked barplot status
#'
#' @param df data.frame
#' @param percent logical to make the y axis in percent
#'
#' @import ggplot2
#'
#' @return ggplot plot
#' @export
stackedbarplot_plot <- function(df, percent =  T) {
  suffix = NULL
  if (percent) {
    df$ratio.over.cases <- 100*df$ratio.over.cases
    suffix = "%"
  }
  p <- df %>%
    ggplot(aes(x = Country.Region, y = ratio.over.cases, fill = status,
               text = paste0("percentage: ", round(ratio.over.cases, 1), suffix,"</br>",
               label = paste("count: ",
                             formatC(countstatus, format = "f", big.mark = ",", digits  = 0)))))+
    basic_plot_theme() +
    geom_col(position = position_stack(reverse = TRUE)) +
    theme(
      axis.text.x = element_text(angle = 30)
    )
  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
  }
  # p = p %>%
  #   fix_colors()
  p
}

#' Time evolution as line plot
#'
#' @rdname time_evol_line_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#'
#' @return line plot of given variable by date
#'
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom  dplyr mutate
#' @importFrom  dplyr case_when
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#' get_timeseries_by_contagion_day_data %>%
#' get_timeseries_global_data()
#'
#' df <- df %>%
#' pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
#'  mutate(status = as.factor(status)) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' \dontrun{
#' country_tot <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data %>%
#'  aggregate_country_data() %>%
#'  top_n(10)
#'
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region %in% country_tot$Country.Region) %>%
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' @export
time_evol_line_plot <- function(df, log = F, text = "") {

  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }

  p <- ggplot(df, aes(x = Date, y = Value, colour = Status, text = paste0(text, ": ", Status))) +
    geom_line(size = 1) +
    basic_plot_theme() +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    theme(
      axis.text.x = element_text(angle = 45),
    )

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  p
}


#' Time evolution as area plot
#'
#' @rdname time_evol_area_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param stack logical for producing a stacked plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#'
#' @return area plot of given variable by date
#'
#' @import ggplot2
#' @importFrom  dplyr group_by
#' @importFrom  dplyr ungroup
#' @importFrom  dplyr arrange
#' @importFrom  dplyr desc
#' @importFrom  dplyr mutate
#' @importFrom  dplyr case_when
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#' get_timeseries_by_contagion_day_data %>%
#' get_timeseries_global_data()
#'
#' df <- df %>%
#' pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
#'  mutate(status = as.factor(status)) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' \dontrun{
#' country_tot <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data %>%
#'  aggregate_country_data() %>%
#'  top_n(10)
#'
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region %in% country_tot$Country.Region) %>%
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' @export
time_evol_area_plot <- function(df, stack = F, log = F, text = "") {

  if (stack) {
    df <- df %>%
      arrange(desc(Status)) %>%
      group_by(Date) %>%
      mutate(ValueMax = cumsum(Value), ValueMin = dplyr::lag(ValueMax, default = 0)) %>%
      ungroup()
  } else {
    df <- df %>%
      mutate(ValueMax = Value, ValueMin = 0)
  }

  if (log) {
    # fix 0s and use lead/lag to make sure there are no gaps
    df <- df %>%
      arrange(Date) %>%
      group_by(Status) %>%
      mutate(
        ValueMax = case_when(
          ValueMax == 0 & dplyr::lag(ValueMax, 1, 0) > 1 ~ 1,
          ValueMax == 0 & dplyr::lead(ValueMax, 1, 0) > 1 ~ 1,
          ValueMax == 0 ~ NA_real_,
          TRUE ~ ValueMax
        ),
        ValueMin = case_when(
          ValueMin = 0 & dplyr::lead(ValueMin, 1, 0) > 1 ~ 1,
          ValueMin = 0 & dplyr::lag(ValueMin, 1, 0) > 1 ~ 1,
          is.na(ValueMax) ~ NA_real_,
          TRUE ~ pmax(1L, ValueMin)
        )
      ) %>%
      ungroup()
  }

  p <- ggplot(df, aes(x = Date, y = Value, text = paste0(text, ": ", Status))) +
    geom_ribbon(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status), size = 1, alpha = 0.5, position = 'identity') +
    # shall we instead go for a step-area done with a (wide) barplot? This would reflect the integer nature of the data
    # geom_crossbar(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status, width = 1.1), size = 0, alpha = 1, position = 'identity') +
    basic_plot_theme() +
    scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    theme(
      axis.text.x = element_text(angle = 45)
    )

  p <- p %>%
    fix_colors()

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  p
}

#' Time evolution as area plot facet
#'
#' @rdname time_evol_line_facet_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log character string log or linear
#'
#' @return area plot by date
#'
#' @import ggplot2
#' @import RColorBrewer
#'
#' @export
time_evol_line_facet_plot <- function(df, log) {

  if (log == "log") {
    df <- df %>%
      mutate(value = ifelse(value == 0, NA, value))
  }

  p <-  ggplot(df, aes(x = date, y = value)) +
    geom_line(aes(colour = Country.Region), size = 2) +
    # geom_area(aes(colour = Country.Region, fill = Country.Region), size = 1, alpha = 0.5, position = 'dodge') +
    basic_plot_theme() +
    # scale_fill_brewer(palette = "Dark2") #+
    scale_color_brewer(palette = "Dark2") +
    scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
    )

  p <- p %>%
    fix_legend_position()

  if (log == "log") {
    p <- p %>%
      add_log_scale()
  }

  p <- p +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- case_colors #c("#dd4b39","black","#00a65a","#3c8dbc")
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid.draw(g)

}

#' Transform y to log scale
#' @rdname add_log_scale
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
add_log_scale <- function(p){
  p <- p + scale_y_log10()  #scale_y_continuous(trans = 'log10')
  p
}

#' Remove legend
#' @rdname remove_legend
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
remove_legend <- function(p){
  p <- p + theme(legend.position = "none")
  p
}

#' Evolution from contagion day
#' @rdname from_contagion_day_bar_plot
#'
#' @param df data.frame with column called Contagion_day and x column to plot
#'
#' @return barplot of given variable by contagion
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region == "Italy")
#'  select(Country.Region, contagion_day, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% from_contagion_day_bar_plot()
#' }
#'
#' @return p ggplot object
#'
#' @export
from_contagion_day_bar_plot <- function(df){
  p <- ggplot(df, aes(x = `Contagion Day`, y = Value, fill = Status)) +
    geom_bar(stat = "identity") +
    basic_plot_theme()
  p
}

#' Evolution from contagion day facet
#'
#' @param df data.frame to plot
#'
#' @return barplot facet
#'
#' @import ggplot2
#' @importFrom grid grid.draw
#'
#' @export
from_contagion_day_bar_facet_plot <- function(df){
  p <- ggplot(df, aes(x = contagion_day, y = value, fill = bool_new)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = new_total_colors) + #c("total" = "#C8C8C8", "new" = "#ea8b5b")) +
    basic_plot_theme() +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))

  p <- p %>%
    fix_legend_position()

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- case_colors #c("#dd4b39","black","#00a65a","#3c8dbc")
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid.draw(g)
}

#' Evolution by_date
#' @rdname date_bar_plot
#'
#' @param df data.frame with column called Date and x column to plot
#'
#' @return barplot of given variable by contagion
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region == "Italy")
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% date_bar_plot()
#' }
#'
#' @export
date_bar_plot <- function(df){
  p <- ggplot(df, aes(x = Date, y = Value, fill = Status)) +
    geom_bar(stat = "identity") +
    basic_plot_theme()
  p
}

#' Fix colors
#' @rdname fix_colors
#'
#' @param p ggplot object
#'
#' @import ggplot2
#' @import RColorBrewer
#'
#' @return p ggplot object
#'
#' @export
fix_colors <- function(p) {
  # "confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_recovered", "new_active"
  p <- p +
    suppressWarnings(scale_color_manual(values = c(case_colors, new_case_colors))) +
    suppressWarnings(scale_fill_manual(values = c(case_colors, new_case_colors)))

  p
}

#' Fix legend position
#' @rdname fix_legend_position
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
fix_legend_position <- function(p){
  p <- p +
    # adjust legend position ref: https://stackoverflow.com/questions/7270900/position-legend-in-first-plot-of-facet
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.background = element_rect(fill = "white", colour = NA))
  p
}


#' plot all countries but highlight first 10
#'
#' @param df data.frame with column called Date and Value column to plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#' @param n_highligth number of elements to highlight
#' @param percent logical to make the y axis in percent
#' @param date_x logical to convert x-axis labels to dates
#'
#'
#' @import ggplot2
#' @import RColorBrewer
#' @import zoo
#'
#' @export
plot_all_highlight <- function(df, log = F, text = "", n_highligth = 10, percent =  F, date_x = F) {

  #clean df for log case
  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }

  # if percentage, multiply by 100
  if (percent) {
    df$Value <- 100*df$Value
  }

  df_highlight <- df %>%
    filter(as.integer(Status) < n_highligth + 1) #pick top n_highligth countries, using factor level (factor is ordered by decreasing Value)

  # rolling weekly average (#80)
  df_highlight <- df_highlight %>% group_by(Status) %>%
    arrange(Date)  %>%
    mutate(Value = zoo::rollapplyr(Value, 7, mean, partial=TRUE))

  df_highlight_max <- df_highlight %>%
    group_by(Status) %>%
    filter(Value == max(Value)) %>%
    ungroup()

  p <- ggplot(df, aes(x = Date, y = Value, colour = Status, text = paste0(text, ": ", Status), x_tooltip = Date, y_tooltip = Value)) +
    # geom_line(size = 1, color = "#bbbdb9", alpha = 0.5) +
    basic_plot_theme() +
    geom_line(data = df_highlight, aes(x = Date, y = Value, colour = Status)) +
    scale_color_brewer(palette = "Dark2")

  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
  }

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  if (date_x) { # mutate x axis to a date format
    p <- p +
      scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
      theme(
        axis.text.x = element_text(angle = 45)
      )
  }

  p

}


#' plot rate as hist
#'
#' @param df data.frame
#' @param color string used to define color
#' @param percent logical to make the y axis in percent
#' @param y_min min value on y axis
#'
#' @import ggplot2
#'
#' @return ggplot plot
#' @export
plot_rate_hist <- function(df, color, percent =  F, y_min = 0) {
  if (percent) {
    df$Value <- 100*df$Value
  }

  p <- ggplot(df, aes(x = Country, y = Value)) +
    geom_bar(stat = "identity", fill = rate_colors[[color]]) +
    basic_plot_theme() +
    coord_cartesian(ylim = c(y_min, max(df$Value))) +
    theme(
      axis.text.x = element_text(angle = 30)
    )

  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%")) #scale_y_continuous(labels = scales::label_percent(accuracy = 1))#scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  p
}

#' scatterplot between prevalence and growth rate
#'
#' @param df data.frame
#' @param med list with median values for x and y
#' @param x.min numeric adjustment for cartesian x axis
#' @param y.min numeric adjustment for cartesian y axis
#'
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom scales label_number
#'
#' @return ggplot plot
#' @export
scatter_plot <- function(df, med, x.min = c(0.9, 1.125), y.min = c(0.99,1.02)) {

  df = df %>% rename(
    growthfactor = starts_with("growth")
  )
  # mean.x = mean(df$prevalence_rate_1M_pop)
  # mean.y = mean(df$growthfactor)
  color_cntry = rep("#E69F00", nrow(df))
  color_cntry[df$prevalence_rate_1M_pop < med$x & df$growthfactor < med$y ] = "darkgreen"
  color_cntry[df$prevalence_rate_1M_pop > med$x & df$growthfactor > med$y ] = "#dd4b39"
  color_cntry[df$prevalence_rate_1M_pop < med$x & df$growthfactor > med$y ] = "yellow3"

  xlim =  c(min(df$prevalence_rate_1M_pop,med$x)- diff(range(df$prevalence_rate_1M_pop,med$x))*(1-x.min[1]),
            max(df$prevalence_rate_1M_pop,med$x)*x.min[2])
  ylim = c(1*y.min[1], max(df$growthfactor, med$y)*y.min[2])
  p <- ggplot(df) +
    basic_plot_theme() +
    scale_x_continuous(labels = label_number(#scale = 1/100,
                                             big.mark = ","
                                             #suffix = "K"
                                             )) +
    scale_y_continuous(limits = c(1, NA), labels = label_number(accuracy = 0.1)) +

    # theme(
    #   axis.text.x = element_text()
    # ) +
    #labs(x="prevalence over 1M", y = "growth factor") +
    geom_point(aes(x = prevalence_rate_1M_pop, y = growthfactor),
               color = color_cntry, size = 1.5) +
    geom_vline(xintercept = med$x, colour = "darkblue", linetype="dotted", size = 0.3) +
    geom_hline(yintercept = med$y, colour = "darkblue", linetype="dotted", size = 0.3) +
    geom_text(aes(x = prevalence_rate_1M_pop, y = growthfactor, label= Country.Region),
              check_overlap = TRUE, color = color_cntry, size = 4) +
    coord_cartesian(ylim = ylim,
                    xlim = xlim)

  p
}

