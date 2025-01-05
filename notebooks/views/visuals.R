## LIBRARIES

box::use(
  readr[read_delim, cols, col_date, write_csv, read_csv],
  dplyr[filter, group_by, summarise, n, select, mutate, case_when, lag, first, ungroup, arrange, pull, inner_join, left_join],
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
df_precovid <- backend$pre_covid_df()




#' @export
vc_pre_covid_plt <- function(variables) {
  
  vc_plt <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(series)) %>%  
    filter(data < "2020-01-01") %>% 
    ggplot( aes(x = data, y = urgencias_mensais)) +
    geom_line(color = "blue") +
    theme_bw() +
    scale_x_date(date_breaks = "1 year", date_labels = "%m %Y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Vila do Conde",
      subtitle = "Monthly emergency occurences",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(vc_plt)
  
}


#' @export
barcelos_pre_covid_plt <- function() {
  
  bar_plt <- df %>% 
    filter(unidade_saude == "Barcelos") %>%
    unnest(cols = c(series)) %>%  
    filter(data < "2020-01-01") %>% 
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
    annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2014-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2016-01-01"), xmax = as.Date("2016-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray") +
    annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2018-12-31"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(bar_plt)
}

#' @export
stationary_tests_table <- function() {
  
  table <- df_precovid %>% 
    select(unidade_saude, adf.returns.test, kpss.returns.test) %>% 
    mutate(
      adf.returns.test = map(adf.returns.test, tidy),
      kpss.returns.test = map(kpss.returns.test, tidy)
    ) %>% 
    unnest(cols = c(adf.returns.test, kpss.returns.test), names_sep = "_") %>% 
    pivot_longer(
      cols = c(-unidade_saude,-adf.returns.test_method, -adf.returns.test_alternative,-kpss.returns.test_method),
      names_to = c(".value", "test"),
      names_sep = "_"
    ) %>% 
    select(-adf.returns.test_method, -adf.returns.test_alternative,-kpss.returns.test_method)
  
  return(table)
  
}

#' @export
acf_pacf_vila_conde <- function(){

  df_precovid %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    pull(na.omit(returns.ts)) %>% 
    .[[1]] %>% 
    ggtsdisplay(main = "Vila do Conde ACF/PACF", theme = theme_bw()) 
    
}

#' @export
acf_pacf_barcelos <- function(){
  
  df_precovid %>% 
    filter(unidade_saude == "Barcelos") %>%
    pull(na.omit(returns.ts)) %>% 
    .[[1]] %>% 
    ggtsdisplay(main = "Barcelos ACF/PACF", theme = theme_bw()) 
  
}

#' @export
stl_vila_conde <- function(){
  
  df_precovid %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    pull(returns.ts) %>% 
    .[[1]] %>% 
    msts(seasonal.periods = c(6,12)) %>%  # Specify multiple seasonal periods
    mstl(lambda = NULL, iterate = 4) %>% 
    autoplot() +
    ggtitle("Vila do Conde")  
  
}

#' @export
stl_barcelos <- function(){
  
  df_precovid %>% 
    filter(unidade_saude == "Barcelos") %>%
    pull(returns.ts) %>% 
    .[[1]] %>% 
    msts(seasonal.periods = c(6,12)) %>%  # Specify multiple seasonal periods
    mstl(lambda = NULL, iterate = 4) %>% 
    autoplot() +
    ggtitle("Barcelos")  
  

}


#' @export
acf_pacf_vila_conde_residuals <- function(){
  
  df_precovid %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    pull(returns.ts) %>% 
    .[[1]] %>% 
    msts(seasonal.periods = c(6,12)) %>%  
    mstl(lambda = NULL, iterate = 4) %>%
    .[, "Remainder"] %>%  
    ggtsdisplay(main = "Vila do Conde ACF/PACF of Residuals", theme = theme_bw())
  
}


#' @export
acf_pacf_barcelos_residuals <- function(){
  
  df_precovid %>% 
    filter(unidade_saude == "Barcelos") %>%
    pull(returns.ts) %>% 
    .[[1]] %>% 
    msts(seasonal.periods = c(6,12)) %>%  
    mstl(lambda = NULL, iterate = 4) %>%
    .[, "Remainder"] %>%  
    ggtsdisplay(main = "Barcelos ACF/PACF of Residuals", theme = theme_bw())
  
}

#' @export
model_fit_plots_viladoconde <- function(){

  vc_ts <- na.omit(df_precovid$returns.ts[[2]])
  astsa::sarima(vc_ts,1,0,1,0,1,1,12)
    
}

#' @export
model_fit_plots_barcelos <- function(){
  
  bc_ts <- na.omit(df_precovid$returns.ts[[1]])
  astsa::sarima(bc_ts ,1,0,1,1,1,1,12)
  
}




