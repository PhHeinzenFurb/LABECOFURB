#importando bibliotecas WDI e tidyverse
library(WDI)
library(tidyverse)
library(scales)

# iremos utilizar dois códigos
# Gross domestic savings (% of GDP)(NY.GDS.TOTL.ZS)
# Gross savings (% of GDP)(NY.GNS.ICTR.ZS)
# "PIB(US$)" = "NY.GDP.MKTP.CD" 

# Ajustando a notacao cientifica
options(scipen = 999)

# Criando lista com os codigos
codigosLista = c( "PIB(US$)" = "NY.GDP.MKTP.CD",
                  "Poupanca_Bruta(%PIB)" = "NY.GNS.ICTR.ZS",
                  "Poupanca_Domestica_Bruta(%PIB)" = "NY.GDS.TOTL.ZS")
# Paises Mercosul
mercosulLista = c("BRA", "URY", "PRY", "ARG", "BOL", "VEN")

# Criando db com dados de PIB, Poupanca_Bruta(%PIB) 
# e Poupanca_Domestica_Bruta(%PIB)
dbPoupancaPib = WDI(indicator = codigosLista, country = "all")

# fazendo corte transversal
dbPoupancaPib2023 = WDI(indicator = codigosLista, country = "all", start = 2023,
                        end = 2023)

# fazendo uma série temporal
dbPoupancaPibMercosul = WDI(indicator = codigosLista, country = mercosulLista,
                            start = 2006, end = 2023)

# Verificando o valor de poupanca da China
dbPoupancaChina = dbPoupancaPib |>
  filter(country == "China")

# selecionando as colunas necessárias em dbPoupancaPIB
dbPoupancaPibMercosul = select(dbPoupancaPibMercosul, -(iso2c:iso3c))

# criando colunas calculadas:
# Poupanca_Bruta (US$) e Poupanca_Domestica_Bruta (US$)
# a partir das porcentagens de cada variável
dbPoupancaPibMercosul["Poupanca_Bruta (US$)"] = 
  dbPoupancaPibMercosul["PIB(US$)"] * (dbPoupancaPibMercosul["Poupanca_Bruta(%PIB)"] / 100)

dbPoupancaPibMercosul["Poupanca_Domestica_Bruta (US$)"] = 
  dbPoupancaPibMercosul["PIB(US$)"] * (dbPoupancaPibMercosul["Poupanca_Domestica_Bruta(%PIB)"] / 100)








