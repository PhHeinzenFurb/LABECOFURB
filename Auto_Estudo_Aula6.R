# chamando livrarias
library(WDI)
library(tidyverse)
library(plotly)
library(gghighlight)

# vetor para códigos do WDI
# NY.ADJ.NNTY.CD
# NY.ADJ.NNTY.KD.ZG
vetorCodigos = c("Renda Nacional Liquida Ajustada Per Capita" = "NY.ADJ.NNTY.PC.CD",
                 "Renda Nacional Liquida Ajustada Per Capita (% cresc. anual)" = "NY.ADJ.NNTY.PC.KD.ZG",
                 "Gastos Consumo Final (% cresc. anual)" = "NE.CON.TOTL.KD.ZG",
                 "Poupanca Bruta (% do PIB)" = "NY.GNS.ICTR.ZS")


#criando tabelas atraves do WDI

# DADOS EM PAINEL
tabelaRenda = WDI(indicator = vetorCodigos, country = "all")

# CORTE TRANSVERSAL
tabelaRenda2023 = WDI(indicator = vetorCodigos, country = "all",
                      start = 2023, end = 2023)

# SERIE TEMPORAL
tabelaRendaBR = WDI(indicator = vetorCodigos, country = "BR",
                    start = 1975, end = 2021)

tabelaRendaMercolsul = WDI(indicator = vetorCodigos, country = "BR",
                           start = 1975, end = 2021)


# Verificando se há relação entre Renda Per Capita e Gastos no Brasil
grafRendaGastos = ggplot(tabelaRendaBR,
                         aes(x = `Renda Nacional Liquida Ajustada Per Capita (% cresc. anual)`,
                             y = `Gastos Consumo Final (% cresc. anual)`)) +
  geom_point() +
  geom_smooth()

ggplotly(grafRendaGastos)

# Verificando a relação entre Renda per Capita e Poupanca
grafRendaPoupanca = ggplot(tabelaRendaBR,
                         aes(x = `Renda Nacional Liquida Ajustada Per Capita`,
                             y = `Poupanca Bruta (% do PIB)`)) +
  geom_point() +
  geom_smooth()

# Verificando o aumento da renda no Brasil por ano
grafRendaAno = ggplot(tabelaRendaBR,
                      aes(x = year,
                          y = `Renda Nacional Liquida Ajustada Per Capita`)) +
  geom_col() +
  # destacando o periodo de 1994 ate 2021 (periodo plano real)
  gghighlight(year >= 1994)

