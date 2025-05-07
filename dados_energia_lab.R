# WEBSITES DE AJUDA
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/#axes
# https://pt.r4ds.hadley.nz/layers.html
# https://r-graph-gallery.com/
# https://rstudio.github.io/cheatsheets/data-visualization.pdf

library(tidyverse)
library(WDI)
library(countrycode)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)

# vetor dos indicadores que serao utilizados no trabalho
# e que ja estao com as colunas nomeadas
vetor_codigos = c("% Acesso a tecnologias e combustiveis limpos para cozimento" = "EG.CFT.ACCS.ZS",
                  "Consumo de energia (Kwh per capita)" = "EG.USE.ELEC.KH.PC",
                  "Exposicao Media a PM2.5" = "EN.ATM.PM25.MC.M3")

#===================
# DADOS EM PAINEL
#===================

df_energia_painel = WDI(country = "all", indicator = vetor_codigos)

#===================
# SERIE TEMPORAL
#===================

df_energia_temporal = WDI(country = "BRA", indicator = vetor_codigos)

#===================
# CORTE TRANSVERSAL
#===================

df_energia_transversal = WDI(country = "all", indicator = vetor_codigos,
                            start = 2020, end = 2020)

# utilizando codelist de countrycode para extrair os iso3c dos paises
lista_iso3c = codelist %>%
  select(continent, iso3c) %>%
  drop_na(iso3c)

# arrumandos os dados para os gráficos

#==============================
# GRÁFICO DOS DADOS EM PAINEL
#==============================

# df para utilização nos gráficos
df_energia_painel_graf = df_energia_painel %>%
  inner_join(lista_iso3c, join_by(iso3c)) %>% # inner_join com lista_iso3c para captar apenas os paises
  drop_na(`Consumo de energia (Kwh per capita)`, `Exposicao Media a PM2.5`,
          `% Acesso a tecnologias e combustiveis limpos para cozimento`) %>% # removendo os valores NA
  group_by(year) %>% # agrupando por ano e continente
  summarise(media_acesso = mean(`% Acesso a tecnologias e combustiveis limpos para cozimento`),
            media_consumo = mean(`Consumo de energia (Kwh per capita)`),
            media_exposicao = mean(`Exposicao Media a PM2.5`)) # calculando media para cada variável

# gráfico verificando a correlação entre acesso a fontes limpas e consumo de energia
graf_painel_acesso_consumo = ggplot(df_energia_painel_graf, 
                                    aes(x = media_acesso, y = media_consumo)) +
  geom_point(color = "#20569C", shape = 18, size = 5, position = "jitter") +
  geom_smooth(color = "#FABC29", fill = "#C0C0C0") +
  theme_light() +
  labs(x = "Média de Acesso a Fontes Renováveis para Cozimento", 
       y = "Média de Consumo Energético Per Capita") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 12, face = "bold"))
  
  
# gráfico verificando a correlação entre acesso a fontes limpas e exposicao a PM2.5
graf_painel_acesso_exposicao = ggplot(df_energia_painel_graf, 
                                    mapping = aes(x = media_acesso, y = media_exposicao)) +
  geom_point() +
  geom_smooth()

# gráfico verificando a correlação entre consumo de energia e exposiçao a PM2.5
graf_painel_consumo_exposicao = ggplot(df_energia_painel_graf, 
                                    mapping = aes(x = media_consumo, y = media_exposicao)) +
  geom_point() +
  geom_smooth()
 
# colocando os tres gráficos criados em apenas um painel
ggarrange(graf_painel_acesso_consumo, 
          ggarrange(graf_painel_acesso_exposicao, graf_painel_consumo_exposicao,
                    ncol = 2, labels = c("B", "C")),
          nrow = 2,
          labels = "A")

#==============================
# GRÁFICO DA SERIE TEMPORAL
#==============================
 
# df para utilizaçao nos gráficos
df_energia_temporal_graf = df_energia_temporal %>%
  drop_na(`Consumo de energia (Kwh per capita)`, `Exposicao Media a PM2.5`) %>% # removendo NA
  select(-c(iso2c, iso3c)) # excluindo colunas iso2c e iso3c
  
# verificando variacao da exposicao a PM2.5 no decorrer do tempo
graf_temporal_exposicao = ggplot(df_energia_temporal_graf, 
                              mapping = aes(x = year, y = `Exposicao Media a PM2.5`)) +
  geom_col()
  
# verificando variacao do consumo de energia no decorrer do tempo
graf_temporal_consumo = ggplot(df_energia_temporal_graf, 
                              mapping = aes(x = year, y = `Consumo de energia (Kwh per capita)`)) +
  geom_col()
  
# colocando os tres gráficos criados em apenas um painel
ggarrange(graf_temporal_exposicao, graf_temporal_consumo,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
  
#==============================
# GRÁFICO DO CORTE TRANSVERSAL
#==============================

world <- ne_countries(scale = "medium", returnclass = "sf")

df_energia_transversal <- df_energia_transversal %>%
  rename(gu_a3 = iso3c)

world <- world %>%
  left_join(df_energia_transversal, join_by(gu_a3))

ggplot(data = world) +
  geom_sf(aes(fill = `Exposicao Media a PM2.5`)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf(aes(fill = `Consumo de energia (Kwh per capita)`)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

ggplot(data = world) +
  geom_sf(aes(fill = `% Acesso a tecnologias e combustiveis limpos para cozimento`)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

  
  
