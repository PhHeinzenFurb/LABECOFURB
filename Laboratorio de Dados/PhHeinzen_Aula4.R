# WDI - WORLD DEVELOPMENT INDICATORS
# BASE DE DADOS DO BANCO MUNDIAL

install.packages("WDI")
library(WDI)

# VIGNETTES - PAGINAS COM ORIENTAÇÕES SOBRE OS PACOTES

WDIsearch("gdp")[1:10,]

#	GDP (current US$)(NY.GDP.MKTP.CD)
# GROSS DOMESTIC PRODUCT (GDP) EM DOLARES NORTE-AMERICANOS
# CODIGO - NY.GDP.MKTP.CD

vetorCodigo = c( "PIB(US$)" = "NY.GDP.MKTP.CD" )

# Ajustando os numeros com notacao cientifica
options(scipen = 999)

# Criando df com dados de PIB
basePib = WDI(indicator = vetorCodigo, country = "all")

basePib2023 = WDI(indicator = vetorCodigo, country = "all", start = 2023,
                  end= 2023)