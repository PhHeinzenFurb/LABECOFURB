# chamando livrarias
library(WDI)
library(tidyverse)
library(plotly)
library(gghighlight)

# vetor para códigos do WDI
# NY.ADJ.NNTY.CD
# NY.ADJ.NNTY.KD.ZG
vetorCodigos = c("Renda Nacional Liquida Per Capita" = "NY.ADJ.NNTY.PC.CD",
                 "Renda Nacional Liquida Per Capita (% anual)" = "NY.ADJ.NNTY.PC.KD.ZG",
                 "Gastos Consumo Final (% anual)" = "NE.CON.TOTL.KD.ZG",
                 "Poupanca Bruta (% PIB)" = "NY.GNS.ICTR.ZS")

vetorMercosul = c("BR","ARG", "PRY", "URY")


#criando tabelas atraves do WDI

# DADOS EM PAINEL
dfRenda = WDI(indicator = vetorCodigos, country = "all")

# CORTE TRANSVERSAL
dfRenda2023 = WDI(indicator = vetorCodigos, country = "all",
                      start = 2023, end = 2023)

# SERIE TEMPORAL
dfRendaBR = WDI(indicator = vetorCodigos, country = "BR",
                    start = 1975, end = 2021)

# SERIE TEMPORAL MERCOSUL
dfRendaMercosul = WDI(indicator = vetorCodigos, country = vetorMercosul,
                          start = 1975, end = 2021)

# Verificando se há relação entre Renda Per Capita e Gastos no Brasil
grafRendaGastos = ggplot(tabelaRendaBR,
                         aes(x = `Renda Nacional Liquida Ajustada Per Capita (% cresc. anual)`,
                             y = `Gastos Consumo Final (% cresc. anual)`)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Renda Nacional Liquida Per Capita",
    y = "Gastos Finais (% do PIB)",
    title = "Relação entre Renda Per Capita e Consumo Final",
    subtitle = "Entre 1975 e 2021"
  )

ggplotly(grafRendaGastos)

# Verificando a relação entre Renda per Capita e Poupanca
grafRendaPoupanca = ggplot(tabelaRendaBR,
                         aes(x = `Renda Nacional Liquida Ajustada Per Capita`,
                             y = `Poupanca Bruta (% do PIB)`)) +
  geom_point() +
  geom_smooth() +
  labs(
    x = "Renda Nacional Liquida Per Capita",
    y = "Poupança Bruta (% do PIB)",
    title = "Relação entre Renda Per Capita e Poupança",
    subtitle = "Entre 1975 e 2021",
    
  )

# Verificando o aumento da renda no Brasil por ano
grafRendaAno = ggplot(tabelaRendaBR,
                      aes(x = year,
                          y = `Renda Nacional Liquida Ajustada Per Capita`)) +
  geom_col() +
  labs(
    x = "Ano",
    y = "Renda Nacional Liquida Per Capita",
    title = "Renda Nacional Liquida Per Capita",
    subtitle = "Entre 1975 e 2021"
  )
  # destacando o periodo de 1994 ate 2021 (periodo plano real)
  gghighlight(year >= 1994)


