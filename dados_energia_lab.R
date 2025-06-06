# WEBSITES DE AJUDA
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/#axes
# https://pt.r4ds.hadley.nz/layers.html
# https://r-graph-gallery.com/
# https://rstudio.github.io/cheatsheets/data-visualization.pdf

library(tidyverse) # biblioteca para modelagem dos dados a
library(WDI) # biblioteca para extracao dados WDI
library(countrycode) # biblioteca com codigos dos paises
library(ggpubr) # biblioteca para utilizar ggarrange()
library(sf) # le e codifica dados geograficos
library(rnaturalearth) # dados de pontos geograficos do mundo
library(rnaturalearthdata) # dados de pontos geograficos do mundo

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
  group_by(year) %>% # agrupando por ano
  summarise(media_acesso = mean(`% Acesso a tecnologias e combustiveis limpos para cozimento`),
            media_consumo = mean(`Consumo de energia (Kwh per capita)`),
            media_exposicao = mean(`Exposicao Media a PM2.5`)) # calculando media para cada variável

# criando funcao graf_painel para facilitar a a criacao dos graficos de cada
# variavel dos dados em painel
graf_painel = function(df, x, y, xlabel, ylabel){
  ggplot(df, 
         aes(x = {{x}}, y = {{y}})) +
    geom_point(color = "#20569C", shape = 18, size = 3, position = "jitter") +
    geom_smooth(color = "#FABC29", fill = "#C0C0C0") +
    theme_light() +
    labs(x = xlabel, 
         y = ylabel) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 8, face = "bold"))
}

# criando gráfico para verificar correlacao entre acesso e consumo
graf_painel_acesso_consumo = graf_painel(df_energia_painel_graf,
                                         media_acesso, 
                                         media_consumo,
                                         "Média do Acesso a Fontes Limpas para Cozimento",
                                         "Média de Consumo Elétrico Per Capita")

# gráfico verificando a correlação entre acesso a fontes limpas e exposicao a PM2.5
graf_painel_acesso_exposicao = graf_painel(df_energia_painel_graf,
                                           media_acesso, 
                                           media_exposicao,
                                           "Média do Acesso a Fontes Limpas para Cozimento",
                                           "Média de Exposição a PM2.5")
  

# gráfico verificando a correlação entre consumo de energia e exposiçao a PM2.5
graf_painel_consumo_exposicao = graf_painel(df_energia_painel_graf,
                                            media_consumo, 
                                            media_exposicao,
                                            "Média do Acesso a Fontes Limpas para Cozimento",
                                            "Média de Consumo Elétrico Per Capita")
 
# colocando os tres gráficos criados em apenas um painel
ggarrange(graf_painel_acesso_consumo, 
          ggarrange(graf_painel_acesso_exposicao, graf_painel_consumo_exposicao,
                    ncol = 2),
          nrow = 2)

#==============================
# GRÁFICO DA SERIE TEMPORAL
#==============================

# df para utilizaçao nos gráficos
df_energia_temporal_graf = df_energia_temporal %>%
  drop_na(`Consumo de energia (Kwh per capita)`, `Exposicao Media a PM2.5`) %>% # removendo NA
  select(-c(iso2c, iso3c)) # excluindo colunas iso2c e iso3c
  
# criando funcao graf_painel para facilitar a a criacao dos graficos de cada
# variavel da serie temporal
graf_temporal = function(df, x, y, xlabel, ylabel){
  ggplot(df, aes(x = {{x}}, y = {{y}})) +
           geom_col(color = "#20569C", fill = "#20569C") +
           theme_light() +
           labs(
             x = xlabel,
             y = ylabel
           ) +
           theme(axis.title = element_text(size = 8, face = "bold"))
}

# verificando variacao da exposicao a PM2.5 no decorrer do tempo
graf_temporal_exposicao = graf_temporal(df_energia_temporal_graf,
                                        year, `Exposicao Media a PM2.5`,
                                        "Ano", "Exposição a PM2.5")
  
# verificando variacao do consumo de energia no decorrer do tempo
graf_temporal_consumo = graf_temporal(df_energia_temporal_graf,
                                      year, `Consumo de energia (Kwh per capita)`,
                                      "Ano", "Consumo de Energia (Kwh per capita)")
  
# colocando os tres gráficos criados em apenas um painel
ggarrange(graf_temporal_exposicao, graf_temporal_consumo,
          ncol = 1, nrow = 2)
  
#==============================
# GRÁFICO DO CORTE TRANSVERSAL
#==============================

# criando a base de dados com a localizacao dos paises
world = ne_countries(scale = "medium", returnclass = "sf")

# alterando nome de iso3c para gu_a3 para permitir
# conexão com world
df_energia_transversal = df_energia_transversal %>%
  rename(gu_a3 = iso3c)

# funcao para realizar o grafico de corte trasnversal
graf_transversal = function(df, x, title){
  ggplot(data = df) +
    geom_sf(aes(fill = {{x}})) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    theme_void() +
    guides(fill = guide_colorbar(title = NULL)) +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
}

# realizando left_join para combinar as tabelas
# df_energia_transversal e world
world = world %>%
  left_join(df_energia_transversal, join_by(gu_a3))

# mapa para exposicao ao pm2.5
graf_transversal_exposicao = graf_transversal(world, `Exposicao Media a PM2.5`,
                                              "Exposição Média a PM2.5(microgramas por metro cubico)")
# mapa para o consumo de energia
graf_transversal_consumo = graf_transversal(world, `Consumo de energia (Kwh per capita)`,
                                            "Consumo de energia (Kwh per capita)")
# mapa para o acesso a fontes limpas
graf_transversal_acesso = graf_transversal(world, 
                                           `% Acesso a tecnologias e combustiveis limpos para cozimento`,
                                           "% Acesso a tecnologias e combustiveis limpos para cozimento")

# colocando os tres gráficos criados em apenas um painel
ggarrange(graf_transversal_acesso, 
          ggarrange(graf_transversal_consumo, graf_transversal_exposicao,
                    ncol = 2),
          nrow = 2)
  
  
