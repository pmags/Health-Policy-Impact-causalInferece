### --------------
#' 
#' Project focus on specific health institutions. 
#' This script imports and extracts only the necessary health insitutions to be 
#' used during the eda process
#' 
### --------------

box::use(
  readr[read_delim, cols, col_date, write_csv],
  dplyr[filter],
  stringr[str_detect],
  janitor[clean_names],
  magrittr[`%>%`]
)


df <- read_delim(
  "data/staging/atendimentos-por-tipo-de-urgencia-hospitalar-link.csv",
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE,
  col_types = cols(Período.Format.2 = col_date(format = "%Y/%m/%d"))
  ) %>% clean_names()

df <- filter(
  df,
  str_detect(instituicao, "Póvoa|Barcelos")
)

write_csv(df, "data/staging/barcelos_povoa_emergencias.csv")
