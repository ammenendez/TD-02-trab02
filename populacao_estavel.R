# ============================================================
# Trabalho — População estável e reprodução
# DF, 2022
#
# ITEM 1: Taxas Específicas de Fecundidade (TEF)
#
# Arquivos de entrada (na mesma pasta deste script):
#   sinasc_df_2022_idade_mae_sexo.csv
#   populacao_feminina_df_2022_15a49.xlsx
#
# ============================================================

rm(list = ls())

# 1. Pacotes -------------------------------------------------
pacotes <- c("readxl", "writexl", "ggplot2")
ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(ausentes) > 0) {
  stop(
    paste0(
      "Instale os pacotes necessários: ",
      paste(ausentes, collapse = ", "),
      "\nComando: install.packages(c(",
      paste(sprintf('"%s"', ausentes), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

# 2. Arquivos de entrada -------------------------------------
arquivo_sinasc <- "sinasc_df_2022_idade_mae_sexo.csv"
arquivo_sidra <- "populacao_feminina_df_2022_15a49.xlsx"

arquivos_ausentes <- c(arquivo_sinasc, arquivo_sidra)[!file.exists(c(arquivo_sinasc, arquivo_sidra))]

if (length(arquivos_ausentes) > 0) {
  stop(
    paste0(
      "Arquivo(s) não encontrado(s): ",
      paste(arquivos_ausentes, collapse = ", "),
      ". Coloque os arquivos e o script na mesma pasta."
    ),
    call. = FALSE
  )
}

# Grupos usados no cálculo da fecundidade.
grupos_fecundos <- c(
  "15 a 19 anos",
  "20 a 24 anos",
  "25 a 29 anos",
  "30 a 34 anos",
  "35 a 39 anos",
  "40 a 44 anos",
  "45 a 49 anos"
)

# 3. Leitura do SINASC ---------------------------------------
# O arquivo usa ponto e vírgula. No TABNET, "-" significa zero absoluto.
sinasc <- read.csv2(
  arquivo_sinasc,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("-", "")
)

if (ncol(sinasc) != 5) {
  stop("O arquivo do SINASC deve possuir exatamente cinco colunas.",
       call. = FALSE)
}

names(sinasc) <- c("idade_mae", "nasc_masc", "nasc_fem", "nasc_ign", "nasc_total")

# Converte somente as colunas de contagem e trata "-" como zero.
colunas_nascimentos <- c("nasc_masc", "nasc_fem", "nasc_ign", "nasc_total")
for (coluna in colunas_nascimentos) {
  sinasc[[coluna]] <- suppressWarnings(as.numeric(sinasc[[coluna]]))
  sinasc[[coluna]][is.na(sinasc[[coluna]])] <- 0
}

sinasc_fecundo <- sinasc[sinasc$idade_mae %in% grupos_fecundos, ]
sinasc_fecundo <- sinasc_fecundo[match(grupos_fecundos, sinasc_fecundo$idade_mae), ]

if (nrow(sinasc_fecundo) != 7 || anyNA(sinasc_fecundo$idade_mae)) {
  stop("Não foi possível localizar os sete grupos de 15 a 49 anos no SINASC.",
       call. = FALSE)
}

# 4. Leitura do SIDRA ----------------------------------------
# As seis primeiras linhas são títulos/cabeçalhos do arquivo original.
sidra <- as.data.frame(readxl::read_excel(
  arquivo_sidra,
  sheet = "Tabela",
  skip = 6,
  col_names = c("idade_mae", "declaracao_idade", "populacao_fem")
))

# Seleciona somente os sete grupos etários fecundos.
sidra <- sidra[sidra$idade_mae %in% grupos_fecundos, ]

# Ordena os grupos conforme a ordem definida em grupos_fecundos.
sidra <- sidra[match(grupos_fecundos, sidra$idade_mae), ]

sidra$populacao_fem <- suppressWarnings(as.numeric(sidra$populacao_fem))

# Validação das populações femininas.
if (nrow(sidra) != 7 ||
    anyNA(sidra$populacao_fem)) {
  stop(paste0(
    "Não foi possível localizar as sete populações ",
    "femininas no SIDRA."
  ),
  call. = FALSE)
}

if (any(sidra$populacao_fem <= 0)) {
  stop("A população feminina deve ser positiva em todos os grupos.",
       call. = FALSE)
}


# 5. União das bases -----------------------------------------
item1_tef <- data.frame(
  idade_mae = grupos_fecundos,
  idade_central = c(17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5),
  n = rep(5, 7),
  nascimentos_masculinos = sinasc_fecundo$nasc_masc,
  nascimentos_femininos = sinasc_fecundo$nasc_fem,
  sexo_ignorado = sinasc_fecundo$nasc_ign,
  nascimentos_totais = sinasc_fecundo$nasc_total,
  populacao_feminina = sidra$populacao_fem,
  stringsAsFactors = FALSE
)

# Conferência dos totais originais do SINASC.
total_sexos_original <-
  item1_tef$nascimentos_masculinos +
  item1_tef$nascimentos_femininos +
  item1_tef$sexo_ignorado

if (any(total_sexos_original !=
        item1_tef$nascimentos_totais)) {
  stop(
    paste0(
      "Os totais do SINASC não coincidem ",
      "com a soma dos nascimentos por sexo."
    ),
    call. = FALSE
  )
}


# 6. Redistribuição dos nascimentos com sexo ignorado --------
# Proporção feminina entre os nascimentos com sexo conhecido
# em cada grupo etário da mãe.
item1_tef$proporcao_feminina_conhecida <-
  item1_tef$nascimentos_femininos /
  (item1_tef$nascimentos_masculinos +
     item1_tef$nascimentos_femininos)

# Redistribui os registros ignorados proporcionalmente.
# O resultado é arredondado para preservar números inteiros.
item1_tef$nascimentos_femininos_ajustados <- round(
  item1_tef$nascimentos_femininos +
    item1_tef$sexo_ignorado *
    item1_tef$proporcao_feminina_conhecida
)

# O restante dos nascimentos é atribuído ao sexo masculino.
# Essa operação garante a preservação do total em cada grupo.
item1_tef$nascimentos_masculinos_ajustados <-
  item1_tef$nascimentos_totais -
  item1_tef$nascimentos_femininos_ajustados

# Conferência da redistribuição.
total_sexos_ajustado <-
  item1_tef$nascimentos_masculinos_ajustados +
  item1_tef$nascimentos_femininos_ajustados

if (any(total_sexos_ajustado !=
        item1_tef$nascimentos_totais)) {
  stop(
    paste0(
      "A redistribuição dos nascimentos com sexo ignorado ",
      "não preservou os totais."
    ),
    call. = FALSE
  )
}


# 7. ITEM 1 — Taxas Específicas de Fecundidade ---------------
# TEF total: nascimentos de ambos os sexos por mulher.
item1_tef$tef_total <-
  item1_tef$nascimentos_totais /
  item1_tef$populacao_feminina

# TEF feminina: nascimentos de filhas após a redistribuição
# dos registros com sexo ignorado.
item1_tef$tef_feminina <-
  item1_tef$nascimentos_femininos_ajustados /
  item1_tef$populacao_feminina

# Versões expressas por mil mulheres.
item1_tef$tef_total_por_mil <-
  1000 * item1_tef$tef_total

item1_tef$tef_feminina_por_mil <-
  1000 * item1_tef$tef_feminina


# 8. Exportação dos resultados -------------------------------
dir.create("resultados", showWarnings = FALSE, recursive = TRUE)

dir.create("figuras", showWarnings = FALSE, recursive = TRUE)

write.csv2(
  item1_tef,
  file.path("resultados", "resultados_item1_tef_df_2022.csv"),
  row.names = FALSE,
  na = ""
)

fonte_metodo <- data.frame(
  campo = c(
    "Local e ano",
    "Nascimentos",
    "População",
    "Faixas",
    "Redistribuição",
    "Definição da TEF total",
    "Definição da TEF feminina"
  ),
  descricao = c(
    "Distrito Federal, 2022",
    paste0("SINASC/DATASUS — nascidos vivos ", "por residência da mãe"),
    paste0("IBGE/SIDRA, Censo Demográfico 2022, ", "Tabela 9514"),
    "Grupos quinquenais de 15 a 49 anos",
    paste0(
      "Os nascimentos com sexo ignorado foram ",
      "redistribuídos proporcionalmente entre os sexos ",
      "em cada grupo etário da mãe, com arredondamento ",
      "e preservação dos totais."
    ),
    paste0(
      "TEF total = nascimentos de ambos os sexos ",
      "/ população feminina do grupo"
    ),
    paste0(
      "TEF feminina = nascimentos femininos ajustados ",
      "/ população feminina do grupo"
    )
  ),
  stringsAsFactors = FALSE
)

writexl::write_xlsx(
  list(item1_tef = item1_tef, fonte_metodo = fonte_metodo),
  file.path("resultados", "resultados_item1_tef_df_2022.xlsx")
)


# 9. Gráfico das TEFs ----------------------------------------
dados_grafico <- rbind(
  data.frame(
    idade_mae = item1_tef$idade_mae,
    tipo = "TEF total",
    tef_por_mil = item1_tef$tef_total_por_mil
  ),
  data.frame(
    idade_mae = item1_tef$idade_mae,
    tipo = "TEF de nascimentos femininos",
    tef_por_mil = item1_tef$tef_feminina_por_mil
  )
)

# Define a ordem correta dos grupos no eixo horizontal.
dados_grafico$idade_mae <- factor(dados_grafico$idade_mae,
                                  levels = grupos_fecundos,
                                  ordered = TRUE)

# Define também a ordem da legenda.
dados_grafico$tipo <- factor(dados_grafico$tipo,
                             levels = c("TEF total", "TEF de nascimentos femininos"))

grafico_tef <- ggplot2::ggplot(dados_grafico,
                               ggplot2::aes(
                                 x = idade_mae,
                                 y = tef_por_mil,
                                 color = tipo,
                                 group = tipo
                               )) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_color_manual(values = c(
    "TEF total" = "#1F4E79",
    "TEF de nascimentos femininos" = "#C55A11"
  )) +
  ggplot2::scale_y_continuous(limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0, 0.08))) +
  ggplot2::labs(
    title = "Taxas específicas de fecundidade — DF, 2022",
    x = "Idade da mãe",
    y = "Taxa específica de fecundidade (por mil mulheres)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = file.path("figuras", "grafico_item1_tef_df_2022.png"),
  plot = grafico_tef,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


# 10. Resultado no console -----------------------------------
resultado_console <- item1_tef[c(
  "idade_mae",
  "nascimentos_totais",
  "nascimentos_femininos_ajustados",
  "populacao_feminina",
  "tef_total_por_mil",
  "tef_feminina_por_mil"
)]

names(resultado_console)[names(resultado_console) ==
                           "nascimentos_femininos_ajustados"] <- "nascimentos_femininos"

print(resultado_console, row.names = FALSE, digits = 6)

cat(
  "\nTotal de nascimentos com sexo ignorado redistribuídos:",
  sum(item1_tef$sexo_ignorado),
  "\n"
)

cat(
  "Total ajustado de nascimentos femininos:",
  sum(item1_tef$nascimentos_femininos_ajustados),
  "\n"
)

cat(
  "Total ajustado de nascimentos masculinos:",
  sum(item1_tef$nascimentos_masculinos_ajustados),
  "\n"
)

cat("Total de nascimentos:",
    sum(item1_tef$nascimentos_totais),
    "\n")

message(
  paste0(
    "Item 1 concluído: Taxas Específicas de Fecundidade ",
    "com redistribuição dos nascimentos de sexo ignorado."
  )
)



# 11. ITEM 2 — Idade média à maternidade ----------------------

numerador_imm <- sum(item1_tef$idade_central * item1_tef$n * item1_tef$tef_total)

denominador_imm <- sum(item1_tef$n * item1_tef$tef_total)

if (denominador_imm <= 0) {
  stop("Não foi possível calcular a idade média à maternidade.", call. = FALSE)
}

idade_media_maternidade <- numerador_imm / denominador_imm

item2_idade_media <- data.frame(indicador = "Idade média à maternidade", valor_anos = idade_media_maternidade)

print(item2_idade_media, row.names = FALSE, digits = 4)

message("Item 2 concluído: Idade média à maternidade.")


# 12. ITEM 3 — Taxa Bruta de Reprodução (TBR) ----------------

# Contribuição de cada grupo etário para a TBR
item1_tef$contribuicao_tbr <-
  item1_tef$n * item1_tef$tef_feminina

# Número médio de filhas por mulher, sem considerar mortalidade
TBR <- sum(item1_tef$contribuicao_tbr)

if (!is.finite(TBR) || TBR < 0) {
  stop("Não foi possível calcular a TBR.", call. = FALSE)
}

item3_tbr <- data.frame(indicador = "Taxa Bruta de Reprodução (TBR)", valor_filhas_por_mulher = TBR)

print(item3_tbr, row.names = FALSE, digits = 4)

message("Item 3 concluído: Taxa Bruta de Reprodução (TBR).")



# 13. ITEM 4 — Taxa Líquida de Reprodução (TLR) --------------

arquivo_tabua_fem <- "tabua_vida_feminina_2022.rds"

if (!file.exists(arquivo_tabua_fem)) {
  stop(
    paste0(
      "O arquivo '",
      arquivo_tabua_fem,
      "' não foi encontrado. ",
      "Coloque-o na mesma pasta do script."
    ),
    call. = FALSE
  )
}

# Lê a tábua de vida feminina
tabua_fem <- readRDS(arquivo_tabua_fem)

colunas_necessarias <- c("x", "lx", "nLx")

if (!all(colunas_necessarias %in% names(tabua_fem))) {
  stop("A tábua feminina deve conter as colunas x, lx e nLx.", call. = FALSE)
}

# Idades iniciais dos grupos de fecundidade: 15, 20, ..., 45
idades_iniciais <- item1_tef$idade_central - 2.5

# Localiza os grupos correspondentes na tábua feminina
posicoes_tabua <- match(idades_iniciais, tabua_fem$x)

if (anyNA(posicoes_tabua)) {
  stop("Não foi possível associar as faixas de fecundidade à tábua feminina.",
       call. = FALSE)
}

# Raiz da tábua de vida
l0 <- tabua_fem$lx[1]

if (!is.finite(l0) || l0 <= 0) {
  stop("A raiz da tábua feminina é inválida.", call. = FALSE)
}

# nLx correspondente às faixas de 15–19 a 45–49 anos
item1_tef$nLx_feminino <- tabua_fem$nLx[posicoes_tabua]

# Proporção média sobrevivente em cada intervalo
item1_tef$sobrevivencia_media <-
  item1_tef$nLx_feminino / (item1_tef$n * l0)

# Contribuição de cada grupo etário para a TLR
item1_tef$contribuicao_tlr <-
  item1_tef$n *
  item1_tef$tef_feminina *
  item1_tef$sobrevivencia_media

# Taxa Líquida de Reprodução
TLR <- sum(item1_tef$contribuicao_tlr)

if (!is.finite(TLR) || TLR < 0) {
  stop("Não foi possível calcular a TLR.", call. = FALSE)
}

if (TLR > TBR + 1e-10) {
  warning("A TLR resultou maior que a TBR; confira a tábua feminina.")
}

item4_tlr <- data.frame(indicador = "Taxa Líquida de Reprodução (TLR)", valor_filhas_por_mulher = TLR)

print(item4_tlr, row.names = FALSE, digits = 4)

message("Item 4 concluído: Taxa líquida de Reprodução (TLR).")


# 14. ITEM 5 — Duração média da geração ----------------------

# Pondera a idade central pela contribuição líquida de cada
# grupo etário para a reprodução
duracao_media_geracao <- sum(item1_tef$idade_central * item1_tef$contribuicao_tlr) / TLR

if (!is.finite(duracao_media_geracao) ||
    duracao_media_geracao <= 0) {
  stop("Não foi possível calcular a duração média da geração.", call. = FALSE)
}

item5_geracao <- data.frame(indicador = "Duração média da geração", valor_anos = duracao_media_geracao)

print(item5_geracao, row.names = FALSE, digits = 4)


# 15. ITEM 6 — Taxa intrínseca de crescimento pela TLR -------

if (!is.finite(TLR) || TLR <= 0) {
  stop("A TLR deve ser positiva para calcular a taxa intrínseca.", call. = FALSE)
}

r_tlr <- log(TLR) / duracao_media_geracao

item6_r_tlr <- data.frame(
  indicador = "Taxa intrínseca de crescimento pela TLR",
  r_anual = r_tlr,
  percentual_ao_ano = 100 * r_tlr,
  por_mil_ao_ano = 1000 * r_tlr
)

print(item6_r_tlr, row.names = FALSE, digits = 5)

message("Item 6 concluído: taxa intrínseca de crescimento estimada pela TLR.")



# 16. ITEM 7 — Taxa intrínseca pela equação de Lotka ---------

# Forma discreta da equação de Lotka:
# soma[exp(-r * idade) * contribuição líquida] = 1

funcao_lotka <- function(r) {
  sum(exp(-r * item1_tef$idade_central) *
        item1_tef$contribuicao_tlr) - 1
}

# Localiza numericamente a raiz da equação
solucao_lotka <- uniroot(f = funcao_lotka,
                         interval = c(-0.10, 0.10),
                         tol = 1e-12)

r_lotka <- solucao_lotka$root

# Conferência: o resultado da equação deve ser próximo de 1
soma_lotka <- sum(exp(-r_lotka * item1_tef$idade_central) *
                    item1_tef$contribuicao_tlr)

if (abs(soma_lotka - 1) > 1e-8) {
  stop("A solução encontrada não satisfaz a equação de Lotka.", call. = FALSE)
}

item7_r_lotka <- data.frame(
  indicador = "Taxa intrínseca de crescimento pela equação de Lotka",
  r_anual = r_lotka,
  percentual_ao_ano = 100 * r_lotka,
  por_mil_ao_ano = 1000 * r_lotka,
  verificacao_lotka = soma_lotka
)

print(item7_r_lotka, row.names = FALSE, digits = 6)

# Comparação entre os dois métodos
comparacao_r <- data.frame(
  metodo = c("Aproximação pela TLR", "Equação de Lotka"),
  r_anual = c(r_tlr, r_lotka),
  percentual_ao_ano = 100 * c(r_tlr, r_lotka),
  por_mil_ao_ano = 1000 * c(r_tlr, r_lotka)
)

print(comparacao_r, row.names = FALSE, digits = 6)

message("Item 7 concluído: Taxa intrínseca pela equação de Lotka.")




# 16. ITEM 8 — Taxa bruta de natalidade da população ------------

# estável intrínseca de ambos os sexos

arquivo_tabua_masc <- "tabua_vida_masculina_2022.rds"

if (!file.exists(arquivo_tabua_masc)) {
  stop(paste0("O arquivo '", arquivo_tabua_masc, "' não foi encontrado."),
       call. = FALSE)
}

tabua_masc <- readRDS(arquivo_tabua_masc)

colunas_estavel <- c("x", "n", "idade", "nLx", "ex")

if (!all(colunas_estavel %in% names(tabua_fem)) ||
    !all(colunas_estavel %in% names(tabua_masc))) {
  stop("As tábuas devem conter x, n, idade, nLx e ex.", call. = FALSE)
}

# Proporção observada dos nascimentos por sexo,
# excluindo sexo ignorado e a linha Total
linhas_sinasc_validas <- sinasc$idade_mae != "Total"

total_nasc_masc <- sum(sinasc$nasc_masc[linhas_sinasc_validas])

total_nasc_fem <- sum(sinasc$nasc_fem[linhas_sinasc_validas])

total_nasc_sexo_conhecido <-
  total_nasc_masc + total_nasc_fem

proporcao_nasc_masc <-
  total_nasc_masc / total_nasc_sexo_conhecido

proporcao_nasc_fem <-
  total_nasc_fem / total_nasc_sexo_conhecido

# Função para calcular os pesos estáveis de cada sexo
calcular_pesos_estaveis <- function(tabua, sexo, proporcao_nascimento) {
  base <- tabua[, colunas_estavel]
  
  base$idade_representativa <- ifelse(is.na(base$n), base$x + base$ex, base$x + base$n / 2)
  
  l0_sexo <- tabua$lx[1]
  
  base$peso_estavel <-
    proporcao_nascimento *
    exp(-r_lotka * base$idade_representativa) *
    base$nLx / l0_sexo
  
  base$sexo <- sexo
  base
}

base_estavel_masc <- calcular_pesos_estaveis(tabua = tabua_masc,
                                             sexo = "Masculino",
                                             proporcao_nascimento = proporcao_nasc_masc)

base_estavel_fem <- calcular_pesos_estaveis(tabua = tabua_fem,
                                            sexo = "Feminino",
                                            proporcao_nascimento = proporcao_nasc_fem)

# Denominador correspondente à população total estável
denominador_natalidade_estavel_total <-
  sum(base_estavel_masc$peso_estavel) +
  sum(base_estavel_fem$peso_estavel)

b_estavel_total <-
  1 / denominador_natalidade_estavel_total

item8_natalidade_estavel <- data.frame(
  indicador = paste("Taxa bruta de natalidade", "da população estável total"),
  taxa_anual = b_estavel_total,
  percentual_ao_ano = 100 * b_estavel_total,
  por_mil_ao_ano = 1000 * b_estavel_total
)

print(item8_natalidade_estavel,
      row.names = FALSE,
      digits = 6)

message(
  "Item 8 concluído: Taxa bruta de natalidade da população estável intrínseca de ambos os sexos."
)


# 17. ITEM 9 — Estrutura etária estável de Lotka -------------

estrutura_masc <- data.frame(
  idade = base_estavel_masc$idade,
  sexo = "Masculino",
  proporcao = (b_estavel_total *
                 base_estavel_masc$peso_estavel)
)

estrutura_fem <- data.frame(
  idade = base_estavel_fem$idade,
  sexo = "Feminino",
  proporcao = (b_estavel_total *
                 base_estavel_fem$peso_estavel)
)

estrutura_estavel_lotka <- rbind(estrutura_masc, estrutura_fem)

estrutura_estavel_lotka$percentual <-
  100 * estrutura_estavel_lotka$proporcao

# Conferência: homens e mulheres juntos devem somar 100%
if (abs(sum(estrutura_estavel_lotka$proporcao) - 1) > 1e-8) {
  stop("A estrutura estável total não soma 100%.", call. = FALSE)
}

write.csv2(
  estrutura_estavel_lotka,
  "estrutura_estavel_lotka_ambos_sexos_2022.csv",
  row.names = FALSE
)

# Valores masculinos negativos apenas para o gráfico
estrutura_estavel_lotka$percentual_grafico <-
  ifelse(
    estrutura_estavel_lotka$sexo == "Masculino",-estrutura_estavel_lotka$percentual,
    estrutura_estavel_lotka$percentual
  )

estrutura_estavel_lotka$idade <- factor(estrutura_estavel_lotka$idade,
                                        levels = tabua_fem$idade,
                                        ordered = TRUE)

limite_piramide <- ceiling(max(abs(
  estrutura_estavel_lotka$percentual_grafico
)) / 2) * 2

grafico_estrutura_lotka <- ggplot2::ggplot(estrutura_estavel_lotka,
                                           ggplot2::aes(x = idade, y = percentual_grafico, fill = sexo)) +
  ggplot2::geom_col(width = 0.9) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    limits = c(-limite_piramide, limite_piramide),
    breaks = seq(-limite_piramide, limite_piramide, by = 2),
    labels = function(x)
      paste0(abs(x), "%"),
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Masculino" = "#1F4E79",
    "Feminino" = "#C55A11"
  )) +
  ggplot2::labs(
    title = paste("Estrutura etária estável", "pelo método de Lotka"),
    subtitle = "DF, condições demográficas de 2022",
    x = "Grupo etário",
    y = "Participação na população estável",
    fill = "Sexo"
    
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = "./figuras/piramide_estrutura_estavel_lotka_df_2022.png",
  plot = grafico_estrutura_lotka,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

message("Estrutura estável de Lotka concluída. ",
        "Soma total: ",
        round(sum(estrutura_estavel_lotka$percentual), 6),
        "%.")

message("Item 9 concluído: Estrutura etária estável de Lotka.")






# 18. ITEM 10 — ESTRUTURA ETÁRIA ESTÁVEL PELA MATRIZ DE LESLIE ------

# Grupos etários quinquenais utilizados na matriz
idades_leslie <- seq(0, 80, by = 5)

grupos_leslie <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80 anos ou mais"
)

numero_grupos <- length(grupos_leslie)

# 1. Sobreviventes lx nas idades exatas
lx_leslie <- tabua_fem$lx[match(idades_leslie, tabua_fem$x)]

if (any(is.na(lx_leslie))) {
  stop("Não foi possível localizar todas as idades ",
       "necessárias na tábua de vida feminina.")
}

# Probabilidade de sobreviver de um grupo ao seguinte
sobrevivencia_leslie <- lx_leslie[-1] /
  lx_leslie[-length(lx_leslie)]


# 2. Permanência no grupo aberto de 80 anos ou mais
ex_80 <- tabua_fem$ex[match(80, tabua_fem$x)]

# Aproximação por risco constante no intervalo aberto
sobrevivencia_80_mais <- exp(-5 / ex_80)

# 3. Fecundidade feminina por grupo etário
fecundidade_leslie <- rep(0, numero_grupos)

# As TEFs femininas correspondem aos grupos 15-19 a 45-49
posicoes_reprodutivas <- match(c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                               grupos_leslie)

# Número de filhas por mulher durante cinco anos
fecundidade_leslie[posicoes_reprodutivas] <-
  item1_tef$n *
  item1_tef$tef_feminina

# 4. Construção da matriz de Leslie

matriz_leslie <- matrix(0, nrow = numero_grupos, ncol = numero_grupos)

# Primeira linha: fecundidade
matriz_leslie[1, ] <- fecundidade_leslie

# Subdiagonal: sobrevivência entre grupos
for (i in 1:(numero_grupos - 1)) {
  matriz_leslie[i + 1, i] <- sobrevivencia_leslie[i]
}

# Permanência no último grupo aberto
matriz_leslie[numero_grupos, numero_grupos] <- sobrevivencia_80_mais

rownames(matriz_leslie) <- grupos_leslie
colnames(matriz_leslie) <- grupos_leslie

# Visualizar a matriz
print(round(matriz_leslie, 6))

# 5. Autovalor e autovetor dominantes

resultado_espectral <- eigen(matriz_leslie)

indice_dominante <- which.max(Re(resultado_espectral$values))

lambda_leslie <- Re(resultado_espectral$values[indice_dominante])

vetor_estavel_feminino <- Re(resultado_espectral$vectors[, indice_dominante])

# Corrigir eventual orientação negativa do autovetor
if (sum(vetor_estavel_feminino) < 0) {
  vetor_estavel_feminino <- -vetor_estavel_feminino
}

# Transformar o autovetor em proporções
estrutura_leslie_feminina <-
  vetor_estavel_feminino /
  sum(vetor_estavel_feminino)

# Taxa intrínseca anual equivalente
r_leslie <- log(lambda_leslie) / 5

resultado_leslie <- data.frame(
  indicador = c(
    "Autovalor dominante de cinco anos",
    "Taxa intrínseca anual pela matriz de Leslie"
  ),
  valor = c(lambda_leslie, r_leslie),
  percentual_ao_ano = c(NA, 100 * r_leslie),
  por_mil_ao_ano = c(NA, 1000 * r_leslie)
)

print(resultado_leslie)

# 6. Estrutura estável feminina

tabela_estrutura_leslie <- data.frame(
  idade = grupos_leslie,
  proporcao = estrutura_leslie_feminina,
  percentual = 100 * estrutura_leslie_feminina
)

print(tabela_estrutura_leslie)

write.csv(
  tabela_estrutura_leslie,
  "./resultados/estrutura_estavel_leslie_feminina_df_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)





# MATRIZ DE LESLIE AMPLIADA PARA OS DOIS SEXOS -------------


# Ler a tábua de vida masculina
tabua_masc <- readRDS("tabua_vida_masculina_2022.rds")

# 1. Proporção dos nascimentos por sexo

total_nascimentos_masculinos <- sum(item1_tef$nascimentos_masculinos, na.rm = TRUE)

total_nascimentos_femininos <- sum(item1_tef$nascimentos_femininos, na.rm = TRUE)

proporcao_masculina_nascimento <-
  total_nascimentos_masculinos /
  (total_nascimentos_masculinos +
     total_nascimentos_femininos)

proporcao_feminina_nascimento <-
  total_nascimentos_femininos /
  (total_nascimentos_masculinos +
     total_nascimentos_femininos)

print(data.frame(
  sexo = c("Masculino", "Feminino"),
  proporcao = c(
    proporcao_masculina_nascimento,
    proporcao_feminina_nascimento
  )
))

# 2. Fecundidade masculina equivalente

# A primeira linha da matriz feminina já representa filhas.
# Convertemos para filhos homens usando a razão entre os sexos.

fecundidade_masculina_leslie <-
  fecundidade_leslie *
  (proporcao_masculina_nascimento /
     proporcao_feminina_nascimento)

# 3. Sobrevivência masculina

lx_masculino_leslie <- tabua_masc$lx[match(idades_leslie, tabua_masc$x)]

if (any(is.na(lx_masculino_leslie))) {
  stop("Não foi possível localizar todas as idades ",
       "na tábua de vida masculina.")
}

sobrevivencia_masculina_leslie <-
  lx_masculino_leslie[-1] /
  lx_masculino_leslie[-length(lx_masculino_leslie)]

ex_masculino_80 <- tabua_masc$ex[match(80, tabua_masc$x)]

sobrevivencia_masculina_80_mais <-
  exp(-5 / ex_masculino_80)

# 4. Matriz ampliada

numero_estados <- 2 * numero_grupos

matriz_leslie_ambos <- matrix(0, nrow = numero_estados, ncol = numero_estados)

# Posições dos grupos femininos e masculinos
pos_feminino <- 1:numero_grupos

pos_masculino <- (numero_grupos + 1):(2 * numero_grupos)

# Nascimentos femininos gerados pelas mulheres
matriz_leslie_ambos[pos_feminino[1], pos_feminino] <- fecundidade_leslie

# Nascimentos masculinos gerados pelas mulheres
matriz_leslie_ambos[pos_masculino[1], pos_feminino] <- fecundidade_masculina_leslie

# Sobrevivência feminina
for (i in 1:(numero_grupos - 1)) {
  matriz_leslie_ambos[pos_feminino[i + 1], pos_feminino[i]] <- sobrevivencia_leslie[i]
}

# Permanência feminina no grupo aberto
matriz_leslie_ambos[pos_feminino[numero_grupos], pos_feminino[numero_grupos]] <- sobrevivencia_80_mais

# Sobrevivência masculina
for (i in 1:(numero_grupos - 1)) {
  matriz_leslie_ambos[pos_masculino[i + 1], pos_masculino[i]] <- sobrevivencia_masculina_leslie[i]
}

# Permanência masculina no grupo aberto
matriz_leslie_ambos[pos_masculino[numero_grupos], pos_masculino[numero_grupos]] <- sobrevivencia_masculina_80_mais

# Nomes das linhas e colunas
nomes_estados <- c(paste0("F_", grupos_leslie), paste0("M_", grupos_leslie))

rownames(matriz_leslie_ambos) <- nomes_estados
colnames(matriz_leslie_ambos) <- nomes_estados

# 5. Autovalor e autovetor dominantes

resultado_espectral_ambos <- eigen(matriz_leslie_ambos)

indice_dominante_ambos <- which.max(Re(resultado_espectral_ambos$values))

lambda_leslie_ambos <- Re(resultado_espectral_ambos$values[indice_dominante_ambos])

vetor_estavel_ambos <- Re(resultado_espectral_ambos$vectors[, indice_dominante_ambos])

if (sum(vetor_estavel_ambos) < 0) {
  vetor_estavel_ambos <- -vetor_estavel_ambos
}

vetor_estavel_ambos <-
  vetor_estavel_ambos /
  sum(vetor_estavel_ambos)

r_leslie_ambos <- log(lambda_leslie_ambos) / 5

print(data.frame(
  indicador = c("Autovalor dominante", "Taxa intrínseca anual"),
  valor = c(lambda_leslie_ambos, r_leslie_ambos),
  percentual_ao_ano = c(NA, 100 * r_leslie_ambos)
))

# 6. Tabela da estrutura estável por sexo e idade

estrutura_leslie_ambos <- data.frame(
  idade = rep(grupos_leslie, times = 2),
  sexo = rep(c("Feminino", "Masculino"), each = numero_grupos),
  proporcao = vetor_estavel_ambos
)

estrutura_leslie_ambos$percentual <-
  100 * estrutura_leslie_ambos$proporcao

estrutura_leslie_ambos$percentual_grafico <-
  ifelse(
    estrutura_leslie_ambos$sexo == "Masculino",-estrutura_leslie_ambos$percentual,
    estrutura_leslie_ambos$percentual
  )

estrutura_leslie_ambos$idade <- factor(estrutura_leslie_ambos$idade, levels = grupos_leslie)

print(estrutura_leslie_ambos)

write.csv(
  estrutura_leslie_ambos,
  "./resultados/estrutura_estavel_leslie_ambos_sexos_df_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 7. Pirâmide etária estável pela matriz de Leslie

limite_leslie <- ceiling(max(abs(
  estrutura_leslie_ambos$percentual_grafico
)) / 2) * 2

grafico_estrutura_leslie <- ggplot2::ggplot(estrutura_leslie_ambos,
                                            ggplot2::aes(x = idade, y = percentual_grafico, fill = sexo)) +
  ggplot2::geom_col(width = 0.9) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    limits = c(-limite_leslie, limite_leslie),
    breaks = seq(-limite_leslie, limite_leslie, by = 2),
    labels = function(x) {
      paste0(abs(x), "%")
    },
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Masculino" = "#1F4E79",
    "Feminino" = "#C55A11"
  )) +
  ggplot2::labs(
    title = paste("Estrutura etária estável", "pelo método de Leslie"),
    subtitle = paste("DF, condições demográficas de 2022"),
    x = "Grupo etário",
    y = "Participação na população estável",
    fill = "Sexo"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = paste0(
    "./figuras/piramide_estrutura_estavel_",
    "leslie_df_2022.png"
  ),
  plot = grafico_estrutura_leslie,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

message(
  "Item 10 concluído: Estrutura etária estável de ambos os sexos pelo método matricial de Leslie."
)

# ESTRUTURA ETÁRIA OBSERVADA — DF, 2022. -------------

# Grupos compatíveis com a matriz de Leslie
grupos_observados <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80 anos ou mais"
)

idades_inicio_observadas <- seq(0, 80, by = 5)

# 1. Função para agrupar a população da tábua de vida

agrupar_populacao_observada <- function(tabua) {
  populacao <- numeric(length(grupos_observados))
  
  # A tábua separa menos de 1 ano e 1-4 anos.
  # Aqui os dois grupos são reunidos em 0-4.
  populacao[1] <- sum(tabua$nNx[tabua$x %in% c(0, 1)], na.rm = TRUE)
  
  # Grupos quinquenais de 5-9 até 75-79
  for (i in 2:16) {
    idade_inicial <-
      idades_inicio_observadas[i]
    
    populacao[i] <- sum(tabua$nNx[tabua$x == idade_inicial], na.rm = TRUE)
  }
  
  # Grupo aberto
  populacao[17] <- sum(tabua$nNx[tabua$x == 80], na.rm = TRUE)
  
  return(populacao)
}

# 2. População observada por sexo

populacao_feminina_observada <-
  agrupar_populacao_observada(tabua_fem)

populacao_masculina_observada <-
  agrupar_populacao_observada(tabua_masc)

estrutura_observada <- data.frame(
  idade = rep(grupos_observados, times = 2),
  sexo = rep(c("Feminino", "Masculino"), each = length(grupos_observados)),
  populacao = c(populacao_feminina_observada, populacao_masculina_observada)
)

# 3. Percentuais sobre a população total

populacao_total_observada <- sum(estrutura_observada$populacao)

estrutura_observada$proporcao <-
  estrutura_observada$populacao /
  populacao_total_observada

estrutura_observada$percentual <-
  100 * estrutura_observada$proporcao

estrutura_observada$percentual_grafico <-
  ifelse(
    estrutura_observada$sexo == "Masculino",-estrutura_observada$percentual,
    estrutura_observada$percentual
  )

estrutura_observada$idade <- factor(estrutura_observada$idade, levels = grupos_observados)

# Conferências
print(data.frame(
  indicador = c("População feminina", "População masculina", "População total"),
  valor = c(
    sum(populacao_feminina_observada),
    sum(populacao_masculina_observada),
    populacao_total_observada
  )
))

print(estrutura_observada)

cat("\nSoma dos percentuais:",
    sum(estrutura_observada$percentual),
    "%\n")

write.csv(
  estrutura_observada,
  "./resultados/estrutura_etaria_observada_df_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# 4. Pirâmide etária observada

limite_observado <- ceiling(max(abs(estrutura_observada$percentual_grafico)) / 2) * 2

grafico_estrutura_observada <-
  ggplot2::ggplot(estrutura_observada,
                  ggplot2::aes(x = idade, y = percentual_grafico, fill = sexo)) +
  ggplot2::geom_col(width = 0.9) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    limits = c(-limite_observado, limite_observado),
    breaks = seq(-limite_observado, limite_observado, by = 2),
    labels = function(x) {
      paste0(abs(x), "%")
    },
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::scale_fill_manual(values = c(
    "Masculino" = "#1F4E79",
    "Feminino" = "#C55A11"
  )) +
  ggplot2::labs(
    title = "Estrutura etária observada",
    subtitle = "DF, Censo Demográfico de 2022",
    x = "Grupo etário",
    y = "Participação na população observada",
    fill = "Sexo"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor =
      ggplot2::element_blank(),
    panel.grid.major.y =
      ggplot2::element_blank(),
    plot.title =
      ggplot2::element_text(face = "bold"),
    plot.margin = ggplot2::margin(
      t = 10,
      r = 15,
      b = 10,
      l = 25
    )
  )

ggplot2::ggsave(
  filename = paste0("./figuras/piramide_estrutura_observada_", "df_2022.png"),
  plot = grafico_estrutura_observada,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)


message("Item 11 concluído: Estrutura etária observada do Distrito Federal em 2022.")


# COMPARAÇÃO DAS ESTRUTURAS ETÁRIAS - OBSERVADA, LOTKA E LESLIE ------------


grupos_comparacao <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80 anos ou mais"
)

# 1. Função para somar os dois sexos

preparar_estrutura_total <- function(dados, metodo, agrupar_zero_quatro = FALSE) {
  dados_temporarios <- dados
  
  dados_temporarios$idade <-
    as.character(dados_temporarios$idade)
  
  # Lotka separava menos de 1 ano e 1-4 anos
  if (agrupar_zero_quatro) {
    dados_temporarios$idade[dados_temporarios$idade %in%
                              c("< 1", "1-4")] <- "0-4"
  }
  
  estrutura_total <- aggregate(percentual ~ idade, data = dados_temporarios, FUN = sum)
  
  estrutura_total$metodo <- metodo
  
  estrutura_total$idade <- factor(estrutura_total$idade, levels = grupos_comparacao)
  
  estrutura_total <- estrutura_total[order(estrutura_total$idade), ]
  
  return(estrutura_total)
}

# 2. Preparar cada estrutura

comparacao_observada <- preparar_estrutura_total(dados = estrutura_observada, metodo = "Observada")

comparacao_lotka <- preparar_estrutura_total(dados = estrutura_estavel_lotka,
                                             metodo = "Lotka",
                                             agrupar_zero_quatro = TRUE)

comparacao_leslie <- preparar_estrutura_total(dados = estrutura_leslie_ambos, metodo = "Leslie")

comparacao_estruturas <- rbind(comparacao_observada, comparacao_lotka, comparacao_leslie)

comparacao_estruturas$metodo <- factor(comparacao_estruturas$metodo,
                                       levels = c("Observada", "Lotka", "Leslie"))

# Conferir se cada distribuição soma 100%
conferencia_comparacao <- aggregate(percentual ~ metodo, data = comparacao_estruturas, FUN = sum)

print(conferencia_comparacao)

# 3. Tabela comparativa em formato largo

tabela_observada <- comparacao_observada[, c("idade", "percentual")]

names(tabela_observada)[2] <-
  "observada"

tabela_lotka <- comparacao_lotka[, c("idade", "percentual")]

names(tabela_lotka)[2] <-
  "lotka"

tabela_leslie <- comparacao_leslie[, c("idade", "percentual")]

names(tabela_leslie)[2] <-
  "leslie"

tabela_comparacao <- merge(tabela_observada, tabela_lotka, by = "idade", all = TRUE)

tabela_comparacao <- merge(tabela_comparacao,
                           tabela_leslie,
                           by = "idade",
                           all = TRUE)

tabela_comparacao$idade <- factor(tabela_comparacao$idade, levels = grupos_comparacao)

tabela_comparacao <- tabela_comparacao[order(tabela_comparacao$idade), ]

# Diferenças em pontos percentuais
tabela_comparacao$diferenca_lotka <- tabela_comparacao$lotka - tabela_comparacao$observada

tabela_comparacao$diferenca_leslie <- tabela_comparacao$leslie - tabela_comparacao$observada

print(tabela_comparacao)

write.csv(
  tabela_comparacao,
  "./resultados/comparacao_estruturas_etarias_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 4. Resumo por grandes grupos etários

comparacao_estruturas$grupo_amplo <-
  ifelse(
    comparacao_estruturas$idade %in%
      grupos_comparacao[1:3],
    "0 a 14 anos",
    ifelse(
      comparacao_estruturas$idade %in%
        grupos_comparacao[4:13],
      "15 a 64 anos",
      "65 anos ou mais"
    )
  )

resumo_grupos_amplos <- aggregate(percentual ~ metodo + grupo_amplo,
                                  data = comparacao_estruturas,
                                  FUN = sum)

resumo_grupos_amplos$grupo_amplo <- factor(
  resumo_grupos_amplos$grupo_amplo,
  levels = c("0 a 14 anos", "15 a 64 anos", "65 anos ou mais")
)

resumo_grupos_amplos <- resumo_grupos_amplos[order(resumo_grupos_amplos$metodo,
                                                   resumo_grupos_amplos$grupo_amplo), ]

print(resumo_grupos_amplos)

write.csv(
  resumo_grupos_amplos,
  "./resultados/comparacao_grandes_grupos_etarios_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 5. Gráfico comparativo

grafico_comparacao_estruturas <-
  ggplot2::ggplot(
    comparacao_estruturas,
    ggplot2::aes(
      x = idade,
      y = percentual,
      color = metodo,
      group = metodo
    )
  ) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::scale_color_manual(values = c(
    "Observada" = "#333333",
    "Lotka" = "#C55A11",
    "Leslie" = "#1F4E79"
  )) +
  ggplot2::scale_y_continuous(
    labels = function(x) {
      paste0(x, "%")
    }
  ) +
  ggplot2::labs(
    title = "Comparação das estruturas etárias",
    subtitle = paste("População observada e estruturas", "estáveis — DF, 2022"),
    x = "Grupo etário",
    y = "Participação na população total",
    color = "Estrutura"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor =
      ggplot2::element_blank(),
    axis.text.x =
      ggplot2::element_text(angle = 45, hjust = 1),
    plot.title =
      ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = paste0("./figuras/comparacao_estruturas_etarias_", "df_2022.png"),
  plot = grafico_comparacao_estruturas,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

print(resumo_grupos_amplos)


message(
  "Item 12 concluído: Comparação entre as estruturas etárias observada, de Lotka e de Leslie."
)


# MOMENTUM POPULACIONAL. ------------
# CENÁRIO DE REPOSIÇÃO IMEDIATA

# 1. Calibrar a fecundidade para lambda = 1


calcular_lambda <- function(fator_fecundidade) {
  matriz_temporaria <- matriz_leslie_ambos
  
  # Nascimentos femininos
  matriz_temporaria[pos_feminino[1], pos_feminino] <-
    matriz_leslie_ambos[pos_feminino[1], pos_feminino] * fator_fecundidade
  
  # Nascimentos masculinos
  matriz_temporaria[pos_masculino[1], pos_feminino] <-
    matriz_leslie_ambos[pos_masculino[1], pos_feminino] * fator_fecundidade
  
  autovalores <- eigen(matriz_temporaria, only.values = TRUE)$values
  
  lambda <- max(Re(autovalores))
  
  return(lambda)
}

# Encontrar o fator que torna lambda igual a 1
fator_reposicao <- uniroot(function(fator) {
  calcular_lambda(fator) - 1
}, interval = c(0.01, 5))$root

cat("\nFator de ajuste da fecundidade:", fator_reposicao, "\n")

# 2. Matriz correspondente ao nível de reposição

matriz_reposicao <- matriz_leslie_ambos

matriz_reposicao[pos_feminino[1], pos_feminino] <-
  matriz_reposicao[pos_feminino[1], pos_feminino] * fator_reposicao

matriz_reposicao[pos_masculino[1], pos_feminino] <-
  matriz_reposicao[pos_masculino[1], pos_feminino] * fator_reposicao

# Conferir o autovalor dominante
autovalores_reposicao <- eigen(matriz_reposicao, only.values = TRUE)$values

lambda_reposicao <- max(Re(autovalores_reposicao))

cat("Lambda no cenário de reposição:", lambda_reposicao, "\n")

# 3. Vetor populacional observado em 2022


obter_populacao_ordenada <- function(sexo_escolhido) {
  dados_sexo <- estrutura_observada[estrutura_observada$sexo == sexo_escolhido, ]
  
  dados_sexo$idade <- as.character(dados_sexo$idade)
  
  return(dados_sexo$populacao[match(grupos_leslie, dados_sexo$idade)])
}

populacao_feminina_inicial <-
  obter_populacao_ordenada("Feminino")

populacao_masculina_inicial <-
  obter_populacao_ordenada("Masculino")

vetor_populacional <- c(populacao_feminina_inicial, populacao_masculina_inicial)

if (any(is.na(vetor_populacional))) {
  stop("Há grupos etários ausentes no vetor ",
       "populacional observado.")
}

populacao_inicial <- sum(vetor_populacional)

# 4. Projetar até a estabilização

numero_maximo_ciclos <- 200
tolerancia <- 1e-8

trajetoria_momentum <- data.frame(
  ciclo = 0,
  ano = 2022,
  populacao = populacao_inicial,
  indice_2022 = 1
)

estrutura_anterior <-
  vetor_populacional /
  sum(vetor_populacional)

ciclo_estabilizacao <- NA

for (ciclo in 1:numero_maximo_ciclos) {
  vetor_populacional <-
    matriz_reposicao %*%
    vetor_populacional
  
  vetor_populacional <-
    as.numeric(vetor_populacional)
  
  populacao_projetada <- sum(vetor_populacional)
  
  estrutura_atual <-
    vetor_populacional /
    populacao_projetada
  
  diferenca_estrutura <- max(abs(estrutura_atual -
                                   estrutura_anterior))
  
  trajetoria_momentum <- rbind(
    trajetoria_momentum,
    data.frame(
      ciclo = ciclo,
      ano = 2022 + 5 * ciclo,
      populacao = populacao_projetada,
      indice_2022 =
        populacao_projetada /
        populacao_inicial
    )
  )
  
  if (diferenca_estrutura <
      tolerancia) {
    ciclo_estabilizacao <- ciclo
    break
  }
  
  estrutura_anterior <- estrutura_atual
}

# 5. Indicadores do momentum

populacao_final <- tail(trajetoria_momentum$populacao, 1)

indice_final <- populacao_final /
  populacao_inicial

variacao_final_percentual <-
  100 * (indice_final - 1)

indice_pico <- which.max(trajetoria_momentum$populacao)

populacao_pico <-
  trajetoria_momentum$populacao[indice_pico]

ano_pico <-
  trajetoria_momentum$ano[indice_pico]

variacao_pico_percentual <-
  100 * (populacao_pico /
           populacao_inicial -
           1)

ano_estabilizacao <- ifelse(is.na(ciclo_estabilizacao), NA, 2022 + 5 * ciclo_estabilizacao)

resultado_momentum <- data.frame(
  indicador = c(
    "População inicial em 2022",
    "População máxima durante a transição",
    "Ano da população máxima",
    "Variação máxima em relação a 2022 (%)",
    "População estacionária final",
    "Índice de momentum",
    "Variação final em relação a 2022 (%)",
    "Ano aproximado de estabilização",
    "Fator de ajuste da fecundidade"
  ),
  valor = c(
    populacao_inicial,
    populacao_pico,
    ano_pico,
    variacao_pico_percentual,
    populacao_final,
    indice_final,
    variacao_final_percentual,
    ano_estabilizacao,
    fator_reposicao
  )
)

print(resultado_momentum)

write.csv(
  resultado_momentum,
  "./resultados/resultado_momentum_populacional_df_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  trajetoria_momentum,
  "./resultados/trajetoria_momentum_populacional_df_2022.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 6. Gráfico da trajetória populacional

grafico_momentum <- ggplot2::ggplot(trajetoria_momentum, ggplot2::aes(x = ano, y = indice_2022)) +
  ggplot2::geom_hline(yintercept = 1,
                      linetype = "dashed",
                      color = "#777777") +
  ggplot2::geom_line(linewidth = 1.1, color = "#1F4E79") +
  ggplot2::geom_point(size = 1.8, color = "#1F4E79") +
  ggplot2::scale_y_continuous(
    labels = function(x) {
      paste0(round(100 * x, 1), "%")
    }
  ) +
  ggplot2::labs(
    title = "Momentum populacional",
    subtitle = paste("DF: cenário hipotético de", "reposição imediata a partir de 2022"),
    x = "Ano",
    y = "População em relação a 2022"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor =
      ggplot2::element_blank(),
    plot.title =
      ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  filename = paste0("./figuras/momentum_populacional_df_2022.png"),
  plot = grafico_momentum,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)


options(scipen = 999)
print(resultado_momentum, row.names = FALSE, digits = 8)
