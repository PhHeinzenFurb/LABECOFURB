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
# ESTUDO DE CONVERGÊNCIA - Usando índice de Gini no painel de dados
# =========================
# Selecionar dados de Gini e preparar painel
painelGini <- dfDesenvolvimentoPainel %>%
  select(iso2c, country, year, gini_index) %>%
  filter(!is.na(gini_index))

# Criar variável dependente: crescimento log do índice de Gini
painelGini <- painelGini %>%
  group_by(iso2c) %>%
  arrange(year) %>%
  mutate(
    log_gini = log(gini_index),
    log_gini_lag = lag(log_gini),
    gini_growth = log_gini - log_gini_lag
  ) %>%
  ungroup()

# Regressão de convergência absoluta: crescimento vs nível inicial
painelConvergencia <- painelGini %>%
  filter(!is.na(gini_growth), !is.na(log_gini_lag))

modelo_convergencia <- lm(gini_growth ~ log_gini_lag, data = painelConvergencia)
summary(modelo_convergencia)

# Gráfico da convergência
ggplot(painelConvergencia, aes(x = log_gini_lag, y = gini_growth)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Convergência do Índice de Gini",
    x = "Log do Índice de Gini no ano anterior",
    y = "Crescimento do Índice de Gini"
  )

# =========================
# RANKINGS COMPARATIVOS - Corte transversal 2023
# =========================
# Função auxiliar para gerar gráficos de ranking
plot_ranking <- function(df, indicador, titulo, cor = "#2c7fb8", eixo_y = "Valor do Indicador") {
  df %>%
    filter(!is.na(.data[[indicador]])) %>%
    arrange(desc(.data[[indicador]])) %>%
    slice_head(n = 20) %>%
    ggplot(aes(x = reorder(country, .data[[indicador]]), y = .data[[indicador]])) +
    geom_col(fill = cor) +
    coord_flip() +
    labs(
      title = titulo,
      x = "País",
      y = eixo_y
    ) +
    theme_minimal()
}

# Gráficos de ranking por indicador
grafico_gini <- plot_ranking(dfDesenvolvimento2023, "gini_index", "Ranking: Índice de Gini (2023)", eixo_y = "Índice de Gini")
grafico_pobreza <- plot_ranking(dfDesenvolvimento2023, "poverty_extreme_pct", "Ranking: Pobreza Extrema (2023)", cor = "#e6550d", eixo_y = "% da População")
grafico_desemprego_total <- plot_ranking(dfDesenvolvimento2023, "unemployment_total_pct", "Ranking: Desemprego Total (2023)", cor = "#31a354", eixo_y = "% da Força de Trabalho")
grafico_desemprego_jovem <- plot_ranking(dfDesenvolvimento2023, "unemployment_youth_pct", "Ranking: Desemprego Jovem (2023)", cor = "#756bb1", eixo_y = "% da População Jovem")

# Exibir gráficos
print(grafico_gini)
print(grafico_pobreza)
print(grafico_desemprego_total)
print(grafico_desemprego_jovem)

# =========================
# ANÁLISE DE SÉRIE TEMPORAL - Evolução dos indicadores na América Latina (últimos 40 anos)
# =========================

# Filtrar apenas os últimos 40 anos (desde 1985)
dfLatAmRecentes <- dfDesenvolvimentoLatAm %>%
  filter(year >= 1985)

# Paleta de cores suaves para países
paleta_suave <- RColorBrewer::brewer.pal(8, "Set2")

# Função auxiliar para obter os top 8 países com mais dados por indicador
top_paises_por_indicador <- function(df, indicador, n = 8) {
  df %>%
    filter(!is.na(.data[[indicador]])) %>%
    count(country, sort = TRUE) %>%
    slice_head(n = n) %>%
    pull(country)
}

# Função para gerar gráfico de série temporal com os principais países
plot_serie_temporal <- function(df, indicador, titulo, eixo_y = "Valor") {
  paises_top <- top_paises_por_indicador(df, indicador)
  df %>%
    filter(country %in% paises_top) %>%
    ggplot(aes(x = year, y = .data[[indicador]], group = country, color = country)) +
    geom_line(size = 1) +
    scale_color_manual(values = rep(paleta_suave, length.out = length(unique(paises_top)))) +
    labs(
      title = titulo,
      x = "Ano",
      y = eixo_y,
      color = "País"
    ) +
    theme_minimal()
}

# Gráficos de séries temporais por indicador
grafico_serie_gini <- plot_serie_temporal(dfLatAmRecentes, "gini_index", "Evolução do Índice de Gini (1985-Atual)", eixo_y = "Índice de Gini")
grafico_serie_pobreza <- plot_serie_temporal(dfLatAmRecentes, "poverty_extreme_pct", "Evolução da Pobreza Extrema (1985-Atual)", eixo_y = "% da População")
grafico_serie_desemprego_total <- plot_serie_temporal(dfLatAmRecentes, "unemployment_total_pct", "Evolução do Desemprego Total (1985-Atual)", eixo_y = "% da Força de Trabalho")
grafico_serie_desemprego_jovem <- plot_serie_temporal(dfLatAmRecentes, "unemployment_youth_pct", "Evolução do Desemprego Jovem (1985-Atual)", eixo_y = "% da População Jovem")

# Exibir gráficos
print(grafico_serie_gini)
print(grafico_serie_pobreza)
print(grafico_serie_desemprego_total)
print(grafico_serie_desemprego_jovem)
