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
  stats[na.omit, window, residuals, ccf, t.test]
)


## SCRIPTS
box::use(
  ../logic/backend
)

df <- backend$import_data()
conclusions_df <- backend$import_conclusions()

df <- df %>% 
  mutate(
    pre_intervention.ts = map(
      .x = returns_corrected.ts,
      .f = ~ window(.x, start = c(2013, 1), end = c(2023, 12))
    ),
    post_intervention.ts = map(
      .x = returns_corrected.ts,
      .f = ~ window(.x, start = c(2024, 1), end = c(2024, 9))
    )
  )

counter_factual.model <- auto.arima(
  na.omit(df$pre_intervention.ts[[2]]), 
  xreg = na.omit(df$pre_intervention.ts[[1]]), 
  seasonal = TRUE
)

#' @export
ccf_plots <- function() {
  
  
  vc <- na.omit(df$pre_intervention.ts[[2]])
  bc <- na.omit(df$pre_intervention.ts[[1]])
  
  fit <- auto.arima(bc)
  
  white_bc <- residuals(fit)
  white_vc <- residuals(Arima(vc, model = fit))
  
  ccf_results <- ccf( white_bc, white_vc, lag.max = 24, main = "Cross-Correlation of Prewhitened Series")
  
}

#' @export
counter_factual_model <- function() {
  
  model <- utils::capture.output({
    astsa::sarima(na.omit(df$pre_intervention.ts[[2]]),1,0,1,1,0,1,12, xreg = na.omit(df$pre_intervention.ts[[1]])
      )
    
  })
  
  
}


#' @export
counter_factual_plot <- function() {
  
  counter_factual.ts <- forecast(counter_factual.model, h =9, xreg = na.omit(df$post_intervention.ts[[1]]))
  
  counter_factual.tibble <- tk_tbl(counter_factual.ts$mean) %>% 
    mutate(date = lubridate::ym(format(index, format = "%y-%m")))
  
  plot <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(returns_corrected.tibble)) %>%  
    mutate(date = lubridate::ym(format(index, format = "%y-%m"))) %>%  
    filter(date >= "2023-01-01") %>% 
    ggplot( ) +
    geom_line(aes(x = date, y = emergencias_mensais, color = "Vila do Conde")) +
    geom_line(
      data = counter_factual.tibble,
      aes(x = date, y = value, color = "Counter Factual")
    ) +
    geom_line(
      data = filter(df, unidade_saude == "Barcelos") %>%
        unnest(cols = c(returns_corrected.tibble)) %>%  
        mutate(date = lubridate::ym(format(index, format = "%y-%m"))) %>% 
        filter(date >= "2024-01-01"),
      aes(x = date, y = emergencias_mensais, color = "Barcelos")
    ) +
    theme_bw() +
    scale_x_date(date_breaks = "1 month", date_labels = "%m %Y") +
    scale_color_manual(
      values = c(
        "Vila do Conde" = "blue",
        "Counter Factual" = "red",
        "Barcelos" = "green"
      )
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "bottom") +
    labs(
      title = "Impact of pre-triage policy",
      subtitle =" Log monthly variations of emergency occurences including counter factual and Barcelos",
      x="",
      y="") +
    annotate("rect", xmin = as.Date("2024-01-01"), xmax = as.Date("2024-09-30"), ymin = -Inf, ymax = Inf, alpha = 0.3, fill = "lightgray")  
  
  return(plot)
}

#' @export
conclusion_test <- function() {
  
  counter_factual.ts <- forecast(counter_factual.model, h =9, xreg = na.omit(df$post_intervention.ts[[1]]))
  
  counter_factual.tibble <- tk_tbl(counter_factual.ts$mean) %>% 
    mutate(date = lubridate::ym(format(index, format = "%y-%m")))
  
  real.tibble <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(returns_corrected.tibble)) %>%  
    mutate(date = lubridate::ym(format(index, format = "%y-%m"))) %>%  
    filter(date >= "2024-01-01") %>% 
    select(emergencias_mensais)
  
  t_test_result <- t.test(counter_factual.tibble$value, real.tibble$emergencias_mensais, alternative = "two.sided", var.equal = FALSE)
  
  return(t_test_result)
  
}

#' @export
conclusion_diff <- function() {
  
  counter_factual.ts <- forecast(counter_factual.model, h =9, xreg = na.omit(df$post_intervention.ts[[1]]))
  
  counter_factual.tibble <- tk_tbl(counter_factual.ts$mean) %>% 
    mutate(date = lubridate::ym(format(index, format = "%y-%m")))
  
  real.tibble <- df %>% 
    filter(unidade_saude == "Vila do Conde") %>%
    unnest(cols = c(returns_corrected.tibble)) %>%  
    mutate(date = lubridate::ym(format(index, format = "%y-%m"))) %>%  
    filter(date >= "2024-01-01") %>% 
    select(emergencias_mensais, date)
  
  join <- left_join(real.tibble, counter_factual.tibble, by = "date") %>% 
    mutate(
      percent.real = (exp(emergencias_mensais) - 1) * 100,
      percent.counterfactual = ( exp(value) - 1) * 100 ,
      diff = percent.real - percent.counterfactual
      )
  
  result <- join %>% summarise(mean(diff))
  
  return(result)
  
}
