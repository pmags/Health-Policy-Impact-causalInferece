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
  stats[na.omit, window]
)


## SCRIPTS
box::use(
  ../logic/backend
)

df <- backend$import_data()

### GENERAL

#' @export
acf_pacf <- function(location) {
  
  df %>% 
    filter(unidade_saude == location) %>%
    unnest(cols = c(series)) %>%  
    pull(na.omit(returns.ts)) %>% 
    .[[1]] %>% 
    window(end = c(2019, 12)) %>%
    ggtsdisplay(main = paste0("Log Variation for ", location," ACF/PACF"), theme = theme_bw()) 
    
}

#' @export
acf_pacf_diff <- function(location){
  
  df %>% 
    filter(unidade_saude == location) %>%
    unnest(cols = c(series)) %>%  
    pull(na.omit(returns.ts)) %>% 
    .[[1]] %>% 
    window(end = c(2019, 12)) %>% 
    diff(lag = 12) %>%
    diff() %>% 
    ggtsdisplay(main = paste0("Log Variation for ", location, " ACF/PACF"), theme = theme_bw())
  
}

#' @export
model_fit_plots <- function(location){
  
  ts <- df %>% 
    filter(unidade_saude == location) %>%
    unnest(cols = c(series)) %>%  
    pull(na.omit(returns.ts)) %>% 
    .[[1]] %>% 
    window(end = c(2019, 12))
  
  model <- utils::capture.output({
    sarima_fit <-ifelse(
      location == "Vila do Conde",
      sarima(ts,1,0,1,0,1,1,12),
      sarima(ts, 4,0,1,1,1,1,12)
    )
     
  })
  
}

#' @export
covid_replacement_plot <- function(location) {
  
  plot <- df %>% 
    filter(unidade_saude == location) %>%
    unnest(cols = c(returns_corrected.tibble)) %>%  
    mutate(date = lubridate::ym(format(index, format = "%y-%m"))) %>%  
    ggplot( aes(x = date, y = emergencias_mensais)) +
    geom_line(color = ifelse(location == "Vila do Conde", "blue", "green")) +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = location,
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
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  +
    annotate("rect", xmin = as.Date("2022-01-01"), xmax = as.Date("2022-12-31"), 
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"), 
             ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plot)
}