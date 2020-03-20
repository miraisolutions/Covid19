#' Time evolution as line plot
#'
#' @rdname time_evol_line_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log logical for applying log scale
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
time_evol_line_plot <- function(df, log = F) {

  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }

  p <- ggplot(df, aes(x = Date, y = Value, colour = Status)) +
    geom_line(size = 1) +
    basic_plot_theme() +
    scale_colour_brewer(palette = "Dark2")

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
#' @param log logical for applying log scale
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
time_evol_area_plot <- function(df, stack = F, log = F) {

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
    df <- df %>%
      mutate(
        ValueMin = ifelse(Value == 0, NA, pmax(1L, ValueMin)),
        ValueMax = ifelse(Value == 0, NA, ValueMax)
      )
  }


  p <- ggplot(df, aes(x = Date, y = Value)) +
    geom_ribbon(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status), size = 1, alpha = 0.5, position = 'identity') +
    # shall we instead go for a step-area done with a (wide) barplot? This would reflect the integer nature of the data
    # geom_crossbar(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status, width = 1.1), size = 0, alpha = 1, position = 'identity') +
    basic_plot_theme()

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
#' @rdname time_evol_area_facet_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log character string log or linear
#'
#' @return area plot by date
#'
#' @import ggplot2
#' @import RColorBrewer
#' @importFrom dplyr mutate
#'
#' @export
time_evol_area_facet_plot <- function(df, log) {

  if (log == "log") {
    df <- df %>%
      mutate(value = ifelse(value == 0, NA, value))
  }

  p <-  ggplot(df, aes(x = date, y = value)) +
    geom_area(aes(colour = Country.Region, fill = Country.Region), size = 2, alpha = 0.5, position = 'dodge') +
    basic_plot_theme() +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2")

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
  fills <- c("#dd4b39","black","#00a65a","#3c8dbc")
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
    scale_fill_manual(values = c("total" = "#C8C8C8", "new" = "#ea8b5b")) +
    basic_plot_theme() +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- c("#dd4b39","black","#00a65a","#3c8dbc")
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
fix_colors <- function(p){

  colors <- c(
    "confirmed" = "#dd4b39",
    "deaths" = "black",
    "recovered" = "#00a65a",
    "active" = "#3c8dbc"
  ) %>% c(stats::setNames(., sprintf("new_%s", names(.))))

  # avoid warnings Scale for 'colour' is already present
  p <- p +
    suppressWarnings(scale_color_manual(values = colors)) +
    suppressWarnings(scale_fill_manual(values = colors))
  p
}
