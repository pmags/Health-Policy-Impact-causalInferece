## LIBRARIES

box::use(
  readr[read_delim, cols, col_date, write_csv, read_csv],
  dplyr[filter, group_by, summarise, n, select, mutate, case_when, lag, first
        , ungroup, arrange, pull, inner_join, left_join],
  tidyr[nest, unnest, pivot_wider, pivot_longer],
  purrr[map, map2],
  timetk[tk_ts, tk_tbl],
  magrittr[`%>%`],
  gridExtra[grid.arrange],
  grid[textGrob, gpar],
  ggplot2[...],
  lubridate[ymd, mdy, dmy, year, month, day, ym],
  forecast[...],
  broom[tidy],
  astsa[...],
  stats[na.omit]
)


## SCRIPTS
box::use(
  ../logic/backend
)

df <- backend$import_data()

#### VILA DO CONDE
#' @export
vc_month_plt <- function() {
  
  vc_plt <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(series)) %>%  
    ggplot( aes(x = data, y = urgencias_mensais) ) +
    geom_line(color = "blue") +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Vila do Conde",
      subtitle = "Monthly emergency occurences",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(vc_plt)
  
}

#' @export
vc_month_maverage_plt <- function() {
  
  plot <- df %>% 
    filter( unidade_saude == "Vila do Conde" ) %>% 
    unnest(cols = c(series)) %>% 
    arrange(data) %>% 
    group_by(unidade_saude) %>% 
    mutate(
      moving_avg_quarter = stats::filter(urgencias_mensais, rep(1/3, 3), sides = 1),
      moving_avg_semester = stats::filter(urgencias_mensais, rep(1/6, 6), sides = 1)
    ) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = data, y = urgencias_mensais, color = "Monthly values"), linetype = "dotted") +
    geom_line(aes(x = data, y = moving_avg_quarter, color = "Quarter Moving Average"), size = 1.2) +
    geom_line(aes(x = data, y = moving_avg_semester, color = "Semester Moving Average"), size = 1.2) +
    theme_bw() +
    labs(
      title = "Moving Average of Urgência Episodes in Póvoa de Varzim/Vila do Conde Health Unit",
      x = "",
      y = "",
      color = NULL
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    scale_color_manual(
      values = c(
        "Monthly values" = "blue", 
        "Quarter Moving Average" = "green", 
        "Semester Moving Average" = "red"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
    
  return(plot)
  
}

#' @export
vc_monthly_returns <- function() {
 
  plt <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(series)) %>%  
    ggplot( aes(x = data, y = series.returns) ) +
    geom_line(color = "blue") +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Vila do Conde",
      subtitle = "Monthly emergency variation",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plt)
   
}

#' @export
vc_month_returns_maverage_plt <- function() {
  
  plot <- df %>% 
    filter( unidade_saude == "Vila do Conde" ) %>% 
    unnest(cols = c(series)) %>% 
    arrange(data) %>% 
    group_by(unidade_saude) %>% 
    mutate(
      moving_avg_quarter = stats::filter(series.returns, rep(1/3, 3), sides = 1),
      moving_avg_semester = stats::filter(series.returns, rep(1/6, 6), sides = 1)
    ) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = data, y = series.returns, color = "Monthly values"), linetype = "dotted") +
    geom_line(aes(x = data, y = moving_avg_quarter, color = "Quarter Moving Average"), size = 1.2) +
    geom_line(aes(x = data, y = moving_avg_semester, color = "Semester Moving Average"), size = 1.2) +
    theme_bw() +
    labs(
      title = "Moving Average of Variation in Urgência Episodes in Vila do Conde Health Unit",
      x = "",
      y = "",
      color = NULL
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    scale_color_manual(
      values = c(
        "Monthly values" = "blue", 
        "Quarter Moving Average" = "green", 
        "Semester Moving Average" = "red"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plot)
  
}



#### BARCELOS
#' @export
barcelos_month_plt <- function() {
  
  bar_plt <- df %>% 
    filter(unidade_saude == "Barcelos") %>%
    unnest(cols = c(series)) %>%  
    ggplot(aes(x = data, y = urgencias_mensais )) +
    geom_line(color = "green") +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Barcelos",
      subtitle = "Monthly emergency occurences",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(bar_plt)
  
}

#' @export
barcelos_month_maverage_plt <- function() {
  
  plot <- df %>% 
    filter( unidade_saude == "Barcelos" ) %>% 
    unnest(cols = c(series)) %>% 
    arrange(data) %>% 
    group_by(unidade_saude) %>% 
    mutate(
      moving_avg_quarter = stats::filter(urgencias_mensais, rep(1/3, 3), sides = 1),
      moving_avg_semester = stats::filter(urgencias_mensais, rep(1/6, 6), sides = 1)
    ) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = data, y = urgencias_mensais, color = "Monthly values"), linetype = "dotted") +
    geom_line(aes(x = data, y = moving_avg_quarter, color = "Quarter Moving Average"), size = 1.2) +
    geom_line(aes(x = data, y = moving_avg_semester, color = "Semester Moving Average"), size = 1.2) +
    theme_bw() +
    labs(
      title = "Moving Average of Urgência Episodes in Barcelos Health Unit",
      x = "",
      y = "",
      color = NULL
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    scale_color_manual(
      values = c(
        "Monthly values" = "blue", 
        "Quarter Moving Average" = "green", 
        "Semester Moving Average" = "red"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plot)
  
}

#' @export
barcelos_monthly_returns <- function() {
  
  plt <- df %>% 
    filter(unidade_saude == "Barcelos") %>%
    unnest(cols = c(series)) %>%  
    ggplot( aes(x = data, y = series.returns) ) +
    geom_line(color = "green") +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Barcelos",
      subtitle = "Monthly emergency variation",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plt)
  
}

#' @export
barcelos_month_returns_maverage_plt <- function() {
  
  plot <- df %>% 
    filter( unidade_saude == "Barcelos" ) %>% 
    unnest(cols = c(series)) %>% 
    arrange(data) %>% 
    group_by(unidade_saude) %>% 
    mutate(
      moving_avg_quarter = stats::filter(series.returns, rep(1/3, 3), sides = 1),
      moving_avg_semester = stats::filter(series.returns, rep(1/6, 6), sides = 1)
    ) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = data, y = series.returns, color = "Monthly values"), linetype = "dotted") +
    geom_line(aes(x = data, y = moving_avg_quarter, color = "Quarter Moving Average"), size = 1.2) +
    geom_line(aes(x = data, y = moving_avg_semester, color = "Semester Moving Average"), size = 1.2) +
    theme_bw() +
    labs(
      title = "Moving Average of Variation in Urgência Episodes in Barcelos Health Unit",
      x = "",
      y = "",
      color = NULL
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    scale_color_manual(
      values = c(
        "Monthly values" = "blue", 
        "Quarter Moving Average" = "green", 
        "Semester Moving Average" = "red"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"),
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plot)
  
}