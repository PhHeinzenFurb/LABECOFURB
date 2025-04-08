library(jsonlite)
library(tidyverse)
library(openxlsx)

link <- "https://servicodados.ibge.gov.br/api/v3/agregados/4636/periodos/2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023|2024/variaveis/5934?localidades=N3[42]"

df_2 <- fromJSON(link, simplifyVector = FALSE)

dados_renda <- tibble(place = df_2)

dados_renda_t <- dados_renda %>%
  unnest_wider(place) %>%
  select(variavel, resultados) %>%
  unnest_longer(resultados) %>%
  unnest_wider(resultados) %>%
  select(variavel, series) %>%
  unnest_longer(series) %>%
  unnest_wider(series)

df_renda_final <- dados_renda_t %>%
  mutate(serie = map(serie, ~ enframe(.x, name = "ano", value = "valor"))) %>%
  unnest(serie) %>%
  select(variavel, ano, valor) %>%
  pivot_wider(names_from = variavel, values_from = valor)
  
write.xlsx(df_renda_final, 
           file = "C:/Users/Usuario/OneDrive/Documents/LABECOFURB/Dados IBGE/Renda_Media_SC.xlsx",
           asTable = TRUE)
