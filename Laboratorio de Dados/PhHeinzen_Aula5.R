#importando pacote WDI e tidyverse
library("WDI")
library("tidyverse")

#formatação para extrair dados de PIB
vetorCodigo = c( "PIB(US$)" = "NY.GDP.MKTP.CD" )

# Ajustando os numeros com notacao cientifica
options(scipen = 999)

# Criando df com dados de PIB
basePib = WDI(indicator = vetorCodigo, country = "all")

basePib2023 = WDI(indicator = vetorCodigo, country = "all", start = 2023,
                        end = 2023)

basePibBrasil = WDI(indicator = vetorCodigo, country = "BRA")

basePibBrasil2023 = WDI(indicator = vetorCodigo, country = "BRA", start = 2023,
                        end = 2023)

basePibNA = basePib |>
              filter(is.na(`PIB(US$)`))

basePibNA %>%
  group_by(country) %>%
  count(country)
  
