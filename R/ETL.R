# Carregue a biblioteca tidyverse
library(tidyverse)

df <- vroom::vroom("X:/Teste/Marcelo/Piramide/piramide.csv")

df |> ggplot2::ggplot() +
  ggplot2::aes(x =  ANO_PREV_APO) +
  geom_histogram(fill = "grey") + 
  ggplot2::theme_minimal()



# Separar ativos e aposentados --------------------------------------------

dados <- df  |> 
  mutate(faixa_etaria = case_when(
    IDADE_SERVIDOR >= 18 & IDADE_SERVIDOR < 25 ~ "18-24 anos",
    IDADE_SERVIDOR >= 25 & IDADE_SERVIDOR < 30 ~ "25-29 anos",
    IDADE_SERVIDOR >= 30 & IDADE_SERVIDOR < 35 ~ "30-34 anos",
    IDADE_SERVIDOR >= 35 & IDADE_SERVIDOR < 40 ~ "35-39 anos",
    IDADE_SERVIDOR >= 40 & IDADE_SERVIDOR < 45 ~ "40-44 anos",
    IDADE_SERVIDOR >= 45 & IDADE_SERVIDOR < 50 ~ "45-49 anos",
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~ "50-54 anos",
    IDADE_SERVIDOR >= 55 & IDADE_SERVIDOR < 60 ~ "55-59 anos",
    IDADE_SERVIDOR >= 60 & IDADE_SERVIDOR < 65 ~ "60-64 anos",
    IDADE_SERVIDOR >= 65 & IDADE_SERVIDOR < 70 ~ "65-69 anos",
    IDADE_SERVIDOR >= 70 & IDADE_SERVIDOR < 75 ~ "70-74 anos",
    IDADE_SERVIDOR >= 75 ~ "75+ anos - Aposentadoria",
    TRUE ~ "Menor de 18 anos"
  ))

ativos <- dados |> dplyr::filter(VAR_0001_SITUACAO =="ATIVO")
aposen <- dados |> dplyr::filter(VAR_0001_SITUACAO =="APOSENTADO")


# Tratamento para dados dos aposentados -----------------------------------

# criar novas faixas

df_aposen <- aposen |>  
  mutate(faixa_etaria_aposen = case_when(
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~ "50-54 anos",
    IDADE_SERVIDOR >= 55 & IDADE_SERVIDOR < 60 ~ "55-59 anos",
    IDADE_SERVIDOR >= 60 & IDADE_SERVIDOR < 65 ~ "60-64 anos",
    IDADE_SERVIDOR >= 65 & IDADE_SERVIDOR < 70 ~ "65-69 anos",
    IDADE_SERVIDOR >= 70 & IDADE_SERVIDOR < 75 ~ "70-74 anos",
    IDADE_SERVIDOR >= 75 & IDADE_SERVIDOR < 80 ~ "75-79 anos",
    IDADE_SERVIDOR >= 80 & IDADE_SERVIDOR < 85 ~ "80-84 anos",
    IDADE_SERVIDOR >= 85 & IDADE_SERVIDOR < 90 ~ "85-89 anos",
    IDADE_SERVIDOR >= 90 ~ "90+ anos",
    TRUE ~ "Menos de 50 anos"
  ))


df_aposen <- df_aposen |>  
  mutate(
    faixa_etaria_aposen = factor(
      faixa_etaria_aposen, levels = c("Menos de 50 anos", "50-54 anos", "55-59 anos", "60-64 anos", "65-69 anos",  "70-74 anos", "75-79 anos", "80-84 anos", "85-89 anos",  "90+ anos")
    )
  ) |> 
  group_by(CO_SEXO) |> 
  dplyr::count(faixa_etaria_aposen, name = "Total") |> 
  dplyr::ungroup()


df_aposen <- df_aposen |>  
  dplyr::mutate(
    Total = ifelse(
      CO_SEXO  == 'M', Total * -1, Total
    )
  )

# Salvar base tratada
saveRDS(df_aposen, "data/df_aposen.rds")


# Tratamento para os dados dos ativos -------------------------------------

# Criar novas faixas de valores baseados na projeção da aposentadoria a cada 5 anos

df_ativos <- ativos |> 
  mutate(faixa_etaria_2028 = case_when(
    IDADE_SERVIDOR >= 18 & IDADE_SERVIDOR < 25 ~ "25-29 anos",
    IDADE_SERVIDOR >= 25 & IDADE_SERVIDOR < 30 ~ "30-34 anos",
    IDADE_SERVIDOR >= 30 & IDADE_SERVIDOR < 35 ~ "35-39 anos",
    IDADE_SERVIDOR >= 35 & IDADE_SERVIDOR < 40 ~ "40-44 anos",
    IDADE_SERVIDOR >= 40 & IDADE_SERVIDOR < 45 ~ "45-49 anos",
    IDADE_SERVIDOR >= 45 & IDADE_SERVIDOR < 50 ~ "50-54 anos",
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~ "55-59 anos",
    IDADE_SERVIDOR >= 55 & IDADE_SERVIDOR < 60 ~ "60-64 anos",
    IDADE_SERVIDOR >= 60 & IDADE_SERVIDOR < 65 ~ "65-69 anos",
    IDADE_SERVIDOR >= 65 & IDADE_SERVIDOR < 70 ~ "70-74 anos",
    IDADE_SERVIDOR >= 70 ~ "75+ anos - Aposentadoria",
    TRUE ~ "Menor de 18 anos"
  ),
  faixa_etaria_2033 = case_when(
    IDADE_SERVIDOR >= 18 & IDADE_SERVIDOR < 25 ~ "30-34 anos",
    IDADE_SERVIDOR >= 25 & IDADE_SERVIDOR < 30 ~ "35-39 anos",
    IDADE_SERVIDOR >= 30 & IDADE_SERVIDOR < 35 ~ "40-44 anos",
    IDADE_SERVIDOR >= 35 & IDADE_SERVIDOR < 40 ~ "45-49 anos",
    IDADE_SERVIDOR >= 40 & IDADE_SERVIDOR < 45 ~ "50-54 anos",
    IDADE_SERVIDOR >= 45 & IDADE_SERVIDOR < 50 ~ "55-59 anos",
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~ "60-64 anos",
    IDADE_SERVIDOR >= 55 & IDADE_SERVIDOR < 60 ~ "65-69 anos",
    IDADE_SERVIDOR >= 60 & IDADE_SERVIDOR < 65 ~ "70-74 anos",
    IDADE_SERVIDOR >= 65 ~ "75+ anos - Aposentadoria",
    TRUE ~ "Menor de 18 anos"
  ),
  faixa_etaria_2033 = case_when(
    IDADE_SERVIDOR >= 18 & IDADE_SERVIDOR < 25 ~ "35-39 anos",
    IDADE_SERVIDOR >= 25 & IDADE_SERVIDOR < 30 ~ "40-44 anos",
    IDADE_SERVIDOR >= 30 & IDADE_SERVIDOR < 35 ~ "45-49 anos",
    IDADE_SERVIDOR >= 35 & IDADE_SERVIDOR < 40 ~ "50-54 anos",
    IDADE_SERVIDOR >= 40 & IDADE_SERVIDOR < 45 ~ "55-59 anos",
    IDADE_SERVIDOR >= 45 & IDADE_SERVIDOR < 50 ~ "60-64 anos",
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~ "65-69 anos",
    IDADE_SERVIDOR >= 55 & IDADE_SERVIDOR < 60 ~ "70-74 anos",
    IDADE_SERVIDOR >= 60 ~ "75+ anos - Aposentadoria",
    TRUE ~ "Menor de 18 anos"
  ),
  faixa_etaria_2038 = case_when(
    IDADE_SERVIDOR >= 18 & IDADE_SERVIDOR < 25 ~ "40-44 anos",
    IDADE_SERVIDOR >= 25 & IDADE_SERVIDOR < 30 ~ "45-49 anos",
    IDADE_SERVIDOR >= 30 & IDADE_SERVIDOR < 35 ~ "50-54 anos",
    IDADE_SERVIDOR >= 35 & IDADE_SERVIDOR < 40 ~ "55-59 anos",
    IDADE_SERVIDOR >= 40 & IDADE_SERVIDOR < 45 ~ "60-64 anos",
    IDADE_SERVIDOR >= 45 & IDADE_SERVIDOR < 50 ~ "65-69 anos",
    IDADE_SERVIDOR >= 50 & IDADE_SERVIDOR < 55 ~  "70-74 anos",
    IDADE_SERVIDOR >= 55 ~ "75+ anos - Aposentadoria",
    TRUE ~ "Menor de 18 anos"
  )
) 

# Fazer a contagem de cada faixa em cada ano e combinar os df's

df_ativos_2023 <- df_ativos |>  
  group_by(CO_SEXO) |> 
  dplyr::count(faixa_etaria, name = "Total") |> 
  dplyr::ungroup() |> 
  mutate(
    Ano = 2023,
    total_ingresso = Total
  )


df_ativos_2028 <- df_ativos |>  
  group_by(CO_SEXO) |> 
  dplyr::count(faixa_etaria_2028, name = "Total") |> 
  dplyr::ungroup() |>
  rename(faixa_etaria = faixa_etaria_2028) |> 
  mutate(
    Ano = 2028
  ) |> mutate(
    total_ingresso = case_when(
      faixa_etaria == "25-29 anos" ~Total + 20000,
      TRUE ~ Total
    )
  )

# Criando uma tibble com as duas novas linhas
novas_linhas <- tribble(
  ~CO_SEXO, ~faixa_etaria, ~Total, ~Ano, ~total_ingresso,
  "F", "25-29 anos", 0, 2033, 20000,
  "M", "25-29 anos", 0, 2033, 20000,
  "F", "30-34 anos", 0, 2033, 20000,
  "M", "30-34 anos", 0, 2033, 20000
)

df_ativos_2033 <- df_ativos |>  
  group_by(CO_SEXO) |> 
  dplyr::count(faixa_etaria_2033, name = "Total") |> 
  dplyr::ungroup() |>
  rename(faixa_etaria = faixa_etaria_2033) |> 
  mutate(
    Ano = 2033
  ) |> mutate(
    total_ingresso = case_when(
      faixa_etaria == "35-39 anos" ~Total + 20000,
      TRUE ~ Total
    )
  )|>  bind_rows(novas_linhas)


# Criando uma tibble com as duas novas linhas
novas_linhas <- tribble(
  ~CO_SEXO, ~faixa_etaria, ~Total, ~Ano, ~total_ingresso,
  "F", "25-29 anos", 0, 2038, 20000,
  "M", "25-29 anos", 0, 2038, 20000,
  "F", "30-34 anos", 0, 2038, 20000,
  "M", "30-34 anos", 0, 2038, 20000,
  "F", "35-39 anos", 0, 2038, 20000,
  "M", "35-39 anos", 0, 2038, 20000
)

df_ativos_2038 <- df_ativos |>  
  group_by(CO_SEXO) |> 
  dplyr::count(faixa_etaria_2038, name = "Total")  |> 
  dplyr::ungroup() |>
  rename(faixa_etaria = faixa_etaria_2038) |> 
  mutate(
    Ano = 2038
  )|> mutate(
    total_ingresso = case_when(
      faixa_etaria == "40-44 anos" ~Total + 20000,
      TRUE ~ Total
    )
  )|>  bind_rows(novas_linhas)

df_final <- dplyr::bind_rows(
  df_ativos_2023, df_ativos_2028, df_ativos_2033, df_ativos_2038
)



df_final <- df_final %>% 
  tidyr::complete(
    CO_SEXO, faixa_etaria,  Ano,
    fill = list(Total = 0, total_ingresso = 0)) 



df_final <- df_final |> 
  dplyr::mutate(
  Total = ifelse(
    CO_SEXO  == 'M', Total * -1, Total
  ),
  total_ingresso = ifelse(
    CO_SEXO  == 'M', total_ingresso * -1, total_ingresso
  )
)



# Salvar base tratada
saveRDS(df_final, "data/df_ativos.rds")
