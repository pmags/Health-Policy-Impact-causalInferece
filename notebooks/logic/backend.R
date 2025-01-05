box::use(
  dplyr[filter], 
  magrittr[`%>%`],
  readr[read_delim, cols, col_date, write_csv, read_csv]
  )

#' @export
import_data <- function() {
  df <- readRDS("../data/curated/dataset.rds")
  return(df)
}

#' @export
pre_covid_df <- function() {
  df <- readRDS("../data/transformed/model_pre_covid.rds")
  return(df)
}
  
  
#' @export
import_conclusions <- function() {
  df <- readRDS("../data/curated/conclusions.rds")
  return(df)
}