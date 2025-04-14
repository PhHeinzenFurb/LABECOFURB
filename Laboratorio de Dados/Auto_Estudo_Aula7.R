# Pacote para acesso aos dados do Banco Mundial
library(WDI)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)

# Vetor nomeado com os códigos dos indicadores de Desenvolvimento Humano e Desigualdade
vetorCodigos <- c(
  gini_index = "SI.POV.GINI",             # Índice de Gini (desigualdade de renda)
  poverty_extreme_pct = "SI.POV.DDAY",    # População abaixo da linha de pobreza extrema (US$ 2,15/dia)
  unemployment_total_pct = "SL.UEM.TOTL.ZS",  # Taxa de desemprego total (% da força de trabalho)
  unemployment_youth_pct = "SL.UEM.1524.ZS"   # Taxa de desemprego entre jovens (15-24 anos)
)

# Vetor com códigos ISO dos países da América Latina
vetorLatAm <- c("AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO", "EC", "SV", "GT",
                "HN", "MX", "NI", "PA", "PY", "PE", "UY", "VE")

# =========================
# DADOS EM PAINEL - Todos os países, todos os anos disponíveis
# =========================
dfDesenvolvimentoPainel <- WDI(
  indicator = vetorCodigos,
  country = "all"
)

# =========================
# CORTE TRANSVERSAL - Todos os países, apenas ano de 2023
# =========================
dfDesenvolvimento2023 <- WDI(
  indicator = vetorCodigos,
  country = "all",
  start = 2023,
  end = 2023
)

# =========================
# SÉRIE TEMPORAL - América Latina, série completa (1960 até o presente)
# =========================
dfDesenvolvimentoLatAm <- WDI(
  indicator = vetorCodigos,
  country = vetorLatAm,
  start = 1960,
  end = as.numeric(format(Sys.Date(), "%Y"))
)

# =========================
# GRÁFICO 1: Painel Temporal - Índice de Gini (Brasil, México, Argentina)
# =========================
df_plot1 <- dfDesenvolvimentoPainel %>%
  filter(iso2c %in% c("BR", "MX", "AR")) %>%
  select(country, year, gini_index) %>%
  drop_na()

ggplot(df_plot1, aes(x = year, y = gini_index, color = country)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Evolução do Índice de Gini (Desigualdade de Renda)",
    x = "Ano",
    y = "Índice de Gini",
    color = "País"
  )

# =========================
# GRÁFICO 2: Corte Transversal - Desemprego Jovem em 2023 (Top 10)
# =========================
df_plot2 <- dfDesenvolvimento2023 %>%
  select(country, unemployment_youth_pct) %>%
  drop_na() %>%
  arrange(desc(unemployment_youth_pct)) %>%
  slice(1:10)

ggplot(df_plot2, aes(x = reorder(country, unemployment_youth_pct), y = unemployment_youth_pct)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(
    title = "Top 10 Países com Maior Desemprego Jovem (2023)",
    x = "País",
    y = "Desemprego Jovem (%)"
  )

# =========================
# GRÁFICO 3: Série Temporal América Latina - Pobreza Extrema no Brasil
# =========================
df_plot3 <- dfDesenvolvimentoLatAm %>%
  filter(iso2c == "BR") %>%
  select(year, poverty_extreme_pct) %>%
  drop_na()

ggplot(df_plot3, aes(x = year, y = poverty_extreme_pct)) +
  geom_area(fill = "steelblue", alpha = 0.7) +
  theme_light(base_size = 14) +
  labs(
    title = "Pobreza Extrema no Brasil ao Longo do Tempo",
    x = "Ano",
    y = "% da População"
  )


