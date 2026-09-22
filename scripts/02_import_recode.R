# 02_import_recode_RAW_SINASC.R
# Lê arquivos nacionais completos do SINASC (2019–2023),
# seleciona apenas as colunas necessárias, filtra Q00–Q07 ano a ano
# e só então combina a coorte. Isso reduz muito o uso de memória.
#
# Arquivos esperados em data/raw/:
# SINASC_2019.csv
# SINASC_2020.csv
# SINASC_2021.csv
# SINASC_2022.csv
# SINASC_2023.csv

library(data.table)
library(dplyr)
library(forcats)
library(stringr)
library(readr)
library(here)

years <- 2019:2023
raw_files <- here("data", "raw", paste0("SINASC_", years, ".csv"))

missing_files <- raw_files[!file.exists(raw_files)]
if (length(missing_files) > 0) {
  stop(
    "Faltam os seguintes arquivos nacionais do SINASC:\n",
    paste(missing_files, collapse = "\n"),
    "\n\nNão renomeie arquivos de outros anos para contornar esta checagem."
  )
}

required_cols <- c(
  "IDADEMAE",
  "ESCMAE",
  "CODMUNRES",
  "GRAVIDEZ",
  "IDANOMAL",
  "CODANOMAL",
  "ESCMAE2010",
  "RACACORMAE",
  "SEMAGESTAC",
  "CONSPRENAT",
  "MESPRENAT",
  "KOTELCHUCK"
)

cohort_list <- vector("list", length(years))
names(cohort_list) <- as.character(years)

for (i in seq_along(years)) {
  yr <- years[i]
  f <- raw_files[i]

  message("\n========================================")
  message("Processando SINASC ", yr)
  message("Arquivo: ", f)
  message("========================================")

  # Lê apenas o cabeçalho para validar a estrutura.
  header <- data.table::fread(
    f,
    nrows = 0,
    encoding = "Latin-1",
    showProgress = FALSE
  )
  available <- names(header)

  missing_cols <- setdiff(required_cols, available)
  if (length(missing_cols) > 0) {
    stop(
      "O arquivo ", basename(f),
      " não contém estas colunas obrigatórias: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Lê somente as colunas necessárias.
  dt <- data.table::fread(
    f,
    select = required_cols,
    encoding = "Latin-1",
    na.strings = c("", "NA", "N/A"),
    showProgress = TRUE
  )

  # Normaliza CODANOMAL como texto e identifica pelo menos um Q00–Q07.
  dt[, CODANOMAL := toupper(as.character(CODANOMAL))]
  dt[, q00_q07 := grepl("Q0[0-7]", CODANOMAL, perl = TRUE)]

  # Filtra ANTES de combinar os anos.
  cohort <- dt[q00_q07 == TRUE]
  cohort[, ANO := yr]

  message(
    "Linhas nacionais lidas: ",
    format(nrow(dt), big.mark = ".", decimal.mark = ",")
  )
  message(
    "Registros Q00–Q07 retidos: ",
    format(nrow(cohort), big.mark = ".", decimal.mark = ",")
  )

  # Mantém apenas a coorte filtrada em memória.
  cohort_list[[i]] <- cohort
  rm(dt, header)
  invisible(gc())
}

cohort_dt <- data.table::rbindlist(
  cohort_list,
  use.names = TRUE,
  fill = TRUE
)

message("\nTotal Q00–Q07 combinado 2019–2023: ",
        format(nrow(cohort_dt), big.mark = ".", decimal.mark = ","))

# Converte para tibble e cria variáveis analíticas.
dat <- cohort_dt |>
  as_tibble() |>
  mutate(
    codanomal_chr = str_to_upper(as.character(CODANOMAL)),

    # Desfecho Kotelchuck:
    # 1 sem pré-natal, 2 inadequado, 3 intermediário = below adequate
    # 4 adequado, 5 mais que adequado = adequate
    # demais = desconhecido/ausente
    outcome = case_when(
      KOTELCHUCK %in% c(1, 2, 3) ~ 1L,
      KOTELCHUCK %in% c(4, 5) ~ 0L,
      TRUE ~ NA_integer_
    ),

    outcome_label = factor(
      outcome,
      levels = c(0, 1),
      labels = c(
        "Adequate or more than adequate",
        "Below adequate"
      )
    ),

    kotelchuck_cat = factor(
      KOTELCHUCK,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "No prenatal care",
        "Inadequate",
        "Intermediate",
        "Adequate",
        "More than adequate"
      )
    ),

    # Raça/cor materna oficial do SINASC.
    race = factor(
      RACACORMAE,
      levels = c(1, 2, 4, 3, 5),
      labels = c("White", "Black", "Brown", "Yellow", "Indigenous")
    ),
    race = fct_relevel(race, "White"),

    # Idade materna.
    age_group = case_when(
      is.na(IDADEMAE) ~ NA_character_,
      IDADEMAE < 20 ~ "<20",
      IDADEMAE <= 34 ~ "20-34",
      IDADEMAE >= 35 ~ ">=35"
    ),
    age_group = factor(
      age_group,
      levels = c("20-34", "<20", ">=35")
    ),

    # Escolaridade materna baseada em ESCMAE.
    education = case_when(
      ESCMAE %in% c(1, 2, 3) ~ "<=7 years",
      ESCMAE == 4 ~ "8-11 years",
      ESCMAE == 5 ~ ">=12 years",
      TRUE ~ NA_character_
    ),
    education = factor(
      education,
      levels = c("8-11 years", "<=7 years", ">=12 years")
    ),

    # Região a partir do primeiro dígito do código IBGE do município.
    codmunres_chr = as.character(CODMUNRES),
    region_code = str_sub(codmunres_chr, 1, 1),
    region = recode(
      region_code,
      "1" = "North",
      "2" = "Northeast",
      "3" = "Southeast",
      "4" = "South",
      "5" = "Central-West",
      .default = NA_character_
    ),
    region = factor(
      region,
      levels = c(
        "Southeast",
        "South",
        "Northeast",
        "North",
        "Central-West"
      )
    ),

    year = factor(ANO, levels = 2019:2023),

    pregnancy_type = case_when(
      GRAVIDEZ == 1 ~ "Singleton",
      GRAVIDEZ %in% c(2, 3) ~ "Multiple",
      TRUE ~ NA_character_
    ),
    pregnancy_type = factor(
      pregnancy_type,
      levels = c("Singleton", "Multiple")
    )
  )

# Salva a coorte derivada; os arquivos brutos nunca são alterados.
out <- here("data", "derived", "sinasc_recoded.rds")
readr::write_rds(dat, out)

# Também salva contagem anual para auditoria imediata.
annual_counts <- dat |>
  count(ANO, name = "n_q00_q07")

readr::write_csv(
  annual_counts,
  here("outputs", "tables", "audit_q00_q07_by_year.csv")
)

print(annual_counts)
message("\nSalvo com sucesso: ", out)
