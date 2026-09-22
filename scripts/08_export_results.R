# 08_export_results.R
# Junta todos os CSVs produzidos em outputs/tables/ em um único arquivo XLSX.

library(tidyverse)
library(here)
library(writexl)

table_files <- list.files(
  path = here("outputs", "tables"),
  pattern = "[.]csv$",
  full.names = TRUE
)

if (length(table_files) == 0) {
  stop(
    "Nenhum CSV foi encontrado em outputs/tables/. ",
    "Rode primeiro os scripts 03 a 07."
  )
}

tables <- purrr::map(
  table_files,
  ~ readr::read_csv(.x, show_col_types = FALSE)
)

sheet_names <- tools::file_path_sans_ext(basename(table_files))
sheet_names <- substr(sheet_names, 1, 31)
names(tables) <- make.unique(sheet_names, sep = "_")

out_file <- here("outputs", "SINASC_reproducibility_results.xlsx")

writexl::write_xlsx(
  tables,
  out_file
)

message("Exportado com sucesso: ", out_file)
