# ============================================================
# Tábua de vida abreviada — DF, 2022
#
# Entrada (na mesma pasta deste script):
#   dados_df_2022.xlsx
#   - aba dados_masc: colunas n, Idade, nDx e nNx
#   - aba dados_fem:  colunas n, Idade, nDx e nNx
#
# Saídas:
#   resultados_tabua_vida_sergipe_2022.xlsx
#   tabua_vida_masculina_2022.csv
#   tabua_vida_feminina_2022.csv
#   tabua_vida_masculina_2022.rds
#   tabua_vida_feminina_2022.rds
# ============================================================

rm(list = ls())

# Pacotes ----------------------------------------------------
pacotes <- c("readxl", "writexl")
ausentes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]

if (length(ausentes) > 0) {
  stop(
    paste0(
      "Instale os pacotes necessários antes de executar o script: ",
      paste(ausentes, collapse = ", "),
      "\nComando: install.packages(c(",
      paste(sprintf('"%s"', ausentes), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

# Arquivo de entrada ----------------------------------------
arquivo_entrada <- "dados_df_2022.xlsx"

if (!file.exists(arquivo_entrada)) {
  stop(
    paste0(
      "O arquivo '", arquivo_entrada, "' não foi encontrado. ",
      "Coloque a planilha e este script na mesma pasta."
    ),
    call. = FALSE
  )
}

# Leitura e validação dos dados brutos ----------------------
ler_dados <- function(aba) {
  dados <- as.data.frame(readxl::read_excel(arquivo_entrada, sheet = aba))
  
  nomes_esperados <- c("n", "Idade", "nDx", "nNx")
  if (!all(nomes_esperados %in% names(dados))) {
    stop(
      paste0(
        "A aba '", aba, "' deve conter as colunas: ",
        paste(nomes_esperados, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  
  dados <- dados[nomes_esperados]
  names(dados) <- c("n", "idade", "nDx", "nNx")
  
  # Remove linhas vazias e uma eventual linha final de Total.
  linha_total <- tolower(trimws(as.character(dados$n))) == "total" |
    tolower(trimws(as.character(dados$idade))) == "total"
  linha_total[is.na(linha_total)] <- FALSE
  dados <- dados[!linha_total, , drop = FALSE]
  dados <- dados[!is.na(dados$idade), , drop = FALSE]
  
  dados$n <- suppressWarnings(as.numeric(dados$n))
  dados$nDx <- suppressWarnings(as.numeric(dados$nDx))
  dados$nNx <- suppressWarnings(as.numeric(dados$nNx))
  
  if (nrow(dados) != 18) {
    stop(
      paste0("A aba '", aba, "' deve possuir exatamente 18 grupos etários."),
      call. = FALSE
    )
  }
  
  if (anyNA(dados$nDx) || anyNA(dados$nNx)) {
    stop(paste0("Há nDx ou nNx ausente/não numérico na aba '", aba, "'."), call. = FALSE)
  }
  
  if (any(dados$nDx < 0) || any(dados$nNx <= 0)) {
    stop(paste0("Na aba '", aba, "', nDx deve ser >= 0 e nNx deve ser > 0."), call. = FALSE)
  }
  
  # O grupo aberto não possui amplitude n definida na planilha.
  dados$n[18] <- NA_real_
  dados
}

# Construção da tábua de vida -------------------------------
construir_tabua <- function(dados, sexo = c("masculino", "feminino")) {
  sexo <- match.arg(sexo)
  dados$x <- c(0, 1, seq(5, 75, by = 5), 80)
  dados <- dados[c("x", "n", "idade", "nDx", "nNx")]
  
  # Taxa central de mortalidade.
  dados$nmx <- dados$nDx / dados$nNx
  
  # Fator de separação: 2,5 nos grupos quinquenais;
  # fórmulas específicas para <1 e 1-4 anos.
  dados$nax <- 2.5
  m0 <- dados$nmx[1]
  
  if (sexo == "masculino") {
    dados$nax[1] <- if (m0 >= 0.107) 0.330 else 0.045 + 2.684 * m0
    dados$nax[2] <- if (m0 >= 0.107) 1.352 else 1.651 - 2.816 * m0
  } else {
    dados$nax[1] <- if (m0 >= 0.107) 0.350 else 0.053 + 2.800 * m0
    dados$nax[2] <- if (m0 >= 0.107) 1.361 else 1.522 - 1.518 * m0
  }
  
  ultima <- nrow(dados)
  if (dados$nmx[ultima] <= 0) {
    stop("O nmx do grupo aberto deve ser maior que zero.", call. = FALSE)
  }
  
  # Grupo aberto: qx = 1 e Lx = lx/mx.
  dados$nax[ultima] <- 1 / dados$nmx[ultima]
  
  dados$nqx <- (dados$n * dados$nmx) /
    (1 + (dados$n - dados$nax) * dados$nmx)
  dados$nqx[ultima] <- 1
  dados$nqx <- pmin(pmax(dados$nqx, 0), 1)
  dados$npx <- 1 - dados$nqx
  
  # Coorte hipotética inicial de 100.000 pessoas.
  dados$lx <- 100000 * cumprod(c(1, head(dados$npx, -1)))
  dados$ndx <- c(-diff(dados$lx), dados$lx[ultima])
  
  dados$nLx <- NA_real_
  dados$nLx[-ultima] <- dados$n[-ultima] * dados$lx[-1] +
    dados$nax[-ultima] * dados$ndx[-ultima]
  dados$nLx[ultima] <- dados$lx[ultima] / dados$nmx[ultima]
  
  dados$Tx <- rev(cumsum(rev(dados$nLx)))
  dados$ex <- dados$Tx / dados$lx
  
  # Conferências internas.
  if (abs(sum(dados$ndx) - 100000) > 1e-6) {
    stop("Falha: a soma de ndx não resultou em 100.000.", call. = FALSE)
  }
  if (any(diff(dados$lx) > 1e-8)) {
    stop("Falha: lx aumentou entre dois grupos etários.", call. = FALSE)
  }
  
  dados
}

# Execução --------------------------------------------------
dados_masc <- ler_dados("dados_masc")
dados_fem <- ler_dados("dados_fem")

tabua_masc <- construir_tabua(dados_masc, "masculino")
tabua_fem <- construir_tabua(dados_fem, "feminino")

resumo <- data.frame(
  sexo = c("Masculino", "Feminino"),
  esperanca_vida_ao_nascer = c(tabua_masc$ex[1], tabua_fem$ex[1]),
  total_obitos_observados = c(sum(tabua_masc$nDx), sum(tabua_fem$nDx)),
  populacao_observada = c(sum(tabua_masc$nNx), sum(tabua_fem$nNx))
)

# Exportações -----------------------------------------------
saveRDS(tabua_masc, "tabua_vida_masculina_2022.rds")
saveRDS(tabua_fem, "tabua_vida_feminina_2022.rds")

write.csv2(
  tabua_masc,
  "tabua_vida_masculina_2022.csv",
  row.names = FALSE,
  na = ""
)
write.csv2(
  tabua_fem,
  "tabua_vida_feminina_2022.csv",
  row.names = FALSE,
  na = ""
)

writexl::write_xlsx(
  list(
    resumo = resumo,
    dados_masc = dados_masc,
    dados_fem = dados_fem,
    tabua_masc = tabua_masc,
    tabua_fem = tabua_fem
  ),
  "tabua_vida_df_2022.xlsx"
)

print(resumo)
message("Tábua de vida concluída.")
