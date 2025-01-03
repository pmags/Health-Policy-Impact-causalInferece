box::use(
  dplyr[filter], 
  magrittr[`%>%`],
  readr[read_delim, cols, col_date, write_csv, read_csv]
  )

#' @export
import_data <- function() {
  pre_covid_df <- readRDS("../data/transformed/model_pre_covid.rds")
  return(pre_covid_df)
}