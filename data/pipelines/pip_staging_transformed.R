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

df <- read_csv("data/staging/barcelos_povoa_emergencias.csv")

df <- 
    df %>% 
    select(data = periodo_format_2, instituicao, urgencias_geral) %>% 
    mutate(
        unidade_saude = case_when(
            str_detect(instituicao, "Varzim") ~ "Vila do Conde",
            TRUE ~ "Barcelos"
        ),
        data = ymd(data)
    ) %>% 
    arrange(data) %>% 
    group_by(unidade_saude, year = year(data)) %>% 
    mutate(urgencias_mensais = ifelse(
        month(data) == 1, 
        urgencias_geral, 
        urgencias_geral - lag(urgencias_geral, default = first(urgencias_geral))) 
    ) %>% 
    ungroup()

write_csv(df, "data/transformed/tratadas_emergencias.csv")
