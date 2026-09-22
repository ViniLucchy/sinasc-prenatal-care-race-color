# 10_sensitivity_analysis_v2.R
# ------------------------------------------------------------
# Sensitivity analyses for the SINASC Q00-Q07 manuscript
# Corrected version:
#   - does NOT require the tidyverse meta-package;
#   - avoids the base-pipe "." error in split();
#   - checks required packages and input files;
#   - preserves the same planned sensitivity analyses.
# ------------------------------------------------------------

# ---------- Project / package checks ----------
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

required_pkgs <- c(
  "dplyr", "purrr", "stringr", "tibble",
  "readr", "here", "sandwich", "lmtest", "writexl"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Missing R package(s): ",
      paste(missing_pkgs, collapse = ", "),
      "\nOpen the SINASC .Rproj first and run scripts/01_setup.R, ",
      "then rerun this script."
    )
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(here)
  library(sandwich)
  library(lmtest)
  library(writexl)
})

# ---------- Helper functions ----------
find_first_existing <- function(df, candidates, object_name = "column") {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(
      paste0(
        "Could not find ", object_name, ". Tried: ",
        paste(candidates, collapse = ", ")
      )
    )
  }
  hit[1]
}

extract_model <- function(model, model_name, vcov_matrix, se_label) {
  ct <- lmtest::coeftest(model, vcov. = vcov_matrix)

  tibble::tibble(
    model = model_name,
    se_type = se_label,
    term = rownames(ct),
    estimate = unname(ct[, 1]),
    std_error = unname(ct[, 2]),
    statistic = unname(ct[, 3]),
    p_value = unname(ct[, 4]),
    pr = exp(estimate),
    conf_low = exp(estimate - 1.96 * std_error),
    conf_high = exp(estimate + 1.96 * std_error)
  )
}

extract_race_terms <- function(df) {
  df |>
    dplyr::filter(stringr::str_detect(term, "^race")) |>
    dplyr::mutate(race = stringr::str_remove(term, "^race")) |>
    dplyr::select(model, se_type, race, pr, conf_low, conf_high, p_value)
}

pct <- function(x, digits = 1) round(100 * x, digits)

# ---------- Paths ----------
analysis_path <- here::here("data", "derived", "analysis_cc.rds")
full_path     <- here::here("data", "derived", "sinasc_recoded.rds")
out_dir       <- here::here("outputs", "tables")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(analysis_path)) {
  stop("analysis_cc.rds not found at: ", analysis_path)
}
if (!file.exists(full_path)) {
  stop("sinasc_recoded.rds not found at: ", full_path)
}

analysis_cc <- readr::read_rds(analysis_path)
sinasc_full <- readr::read_rds(full_path)

# ---------- Detect columns ----------
outcome_col <- find_first_existing(analysis_cc, c("outcome"), "outcome column")
race_col    <- find_first_existing(analysis_cc, c("race"), "race column")
region_col  <- find_first_existing(analysis_cc, c("region"), "region column")
age_col     <- find_first_existing(analysis_cc, c("age_group"), "maternal age-group column")
year_col    <- find_first_existing(analysis_cc, c("year"), "year column")
edu_col     <- find_first_existing(analysis_cc, c("education"), "education column")
cluster_col <- find_first_existing(
  analysis_cc,
  c("CODMUNRES", "codmunres", "cod_mun_res", "mun_res_code"),
  "municipality cluster column"
)

# Relevel only if labels exist; otherwise preserve existing factor ordering.
analysis_cc[[race_col]]   <- as.factor(analysis_cc[[race_col]])
analysis_cc[[region_col]] <- as.factor(analysis_cc[[region_col]])
analysis_cc[[age_col]]    <- as.factor(analysis_cc[[age_col]])
analysis_cc[[year_col]]   <- as.factor(analysis_cc[[year_col]])
analysis_cc[[edu_col]]    <- as.factor(analysis_cc[[edu_col]])

# ---------- Refit main models ----------
formula_m0 <- stats::as.formula(
  paste(outcome_col, "~", race_col)
)

formula_m1 <- stats::as.formula(
  paste(
    outcome_col, "~",
    paste(c(race_col, region_col, age_col, year_col), collapse = " + ")
  )
)

formula_m2 <- stats::as.formula(
  paste(
    outcome_col, "~",
    paste(c(race_col, region_col, age_col, year_col, edu_col), collapse = " + ")
  )
)

m0 <- stats::glm(formula_m0, family = stats::poisson(link = "log"), data = analysis_cc)
m1 <- stats::glm(formula_m1, family = stats::poisson(link = "log"), data = analysis_cc)
m2 <- stats::glm(formula_m2, family = stats::poisson(link = "log"), data = analysis_cc)

# ============================================================
# 1) HC0 vs HC1 vs HC3
# ============================================================
get_hc_results <- function(model, model_name) {
  dplyr::bind_rows(
    extract_model(model, model_name, sandwich::vcovHC(model, type = "HC0"), "HC0"),
    extract_model(model, model_name, sandwich::vcovHC(model, type = "HC1"), "HC1"),
    extract_model(model, model_name, sandwich::vcovHC(model, type = "HC3"), "HC3")
  )
}

hc_all <- dplyr::bind_rows(
  get_hc_results(m0, "M0"),
  get_hc_results(m1, "M1"),
  get_hc_results(m2, "M2")
)

hc_race <- extract_race_terms(hc_all)

hc_m2_compare <- hc_race |>
  dplyr::filter(model == "M2") |>
  dplyr::arrange(race, se_type)

# ============================================================
# 2) Municipality-clustered robust SE
# ============================================================
cluster_var <- analysis_cc[[cluster_col]]

# Remove records with missing cluster id only if any exist.
# The analytical model sample remains otherwise unchanged.
if (any(is.na(cluster_var))) {
  warning(
    sum(is.na(cluster_var)),
    " analytical records have missing municipality code. ",
    "Clustered models will use complete municipality identifiers only."
  )
}

get_cluster_results <- function(model, model_name, cluster_object) {
  V <- sandwich::vcovCL(
    model,
    cluster = cluster_object,
    type = "HC1"
  )
  extract_model(
    model,
    model_name,
    V,
    "Clustered_HC1_municipality"
  )
}

cluster_all <- dplyr::bind_rows(
  get_cluster_results(m0, "M0", cluster_var),
  get_cluster_results(m1, "M1", cluster_var),
  get_cluster_results(m2, "M2", cluster_var)
)

cluster_race <- extract_race_terms(cluster_all)

primary_m2 <- hc_race |>
  dplyr::filter(model == "M2", se_type == "HC0") |>
  dplyr::rename(
    pr_primary = pr,
    conf_low_primary = conf_low,
    conf_high_primary = conf_high,
    p_value_primary = p_value
  ) |>
  dplyr::select(
    model, race,
    pr_primary, conf_low_primary, conf_high_primary, p_value_primary
  )

cluster_m2 <- cluster_race |>
  dplyr::filter(model == "M2") |>
  dplyr::rename(
    pr_cluster = pr,
    conf_low_cluster = conf_low,
    conf_high_cluster = conf_high,
    p_value_cluster = p_value
  ) |>
  dplyr::select(
    model, race,
    pr_cluster, conf_low_cluster, conf_high_cluster, p_value_cluster
  )

cluster_compare_m2 <- dplyr::left_join(
  primary_m2,
  cluster_m2,
  by = c("model", "race")
)

# ============================================================
# 3) Missing-outcome prevalence bounds
# ============================================================
outcome_full_col <- find_first_existing(
  sinasc_full, c("outcome"), "full-data outcome column"
)
region_full_col <- find_first_existing(
  sinasc_full, c("region"), "full-data region column"
)
year_full_col <- find_first_existing(
  sinasc_full, c("year"), "full-data year column"
)

sinasc_full[[region_full_col]] <- as.factor(sinasc_full[[region_full_col]])
sinasc_full[[year_full_col]]   <- as.factor(sinasc_full[[year_full_col]])

calc_bounds <- function(df, group_label = "Overall", group_value = "Overall") {

  n_total <- nrow(df)
  n_missing <- sum(is.na(df[[outcome_full_col]]))
  n_observed <- sum(!is.na(df[[outcome_full_col]]))
  below_observed <- sum(df[[outcome_full_col]] == 1, na.rm = TRUE)
  adequate_observed <- sum(df[[outcome_full_col]] == 0, na.rm = TRUE)

  tibble::tibble(
    group_type = group_label,
    group = as.character(group_value),
    n_total = n_total,
    n_observed_outcome = n_observed,
    n_missing_outcome = n_missing,
    below_observed_n = below_observed,
    adequate_or_more_observed_n = adequate_observed,

    observed_prevalence = below_observed / n_observed,
    observed_prevalence_pct = pct(below_observed / n_observed),

    # Extreme lower bound:
    # every missing Kotelchuck record is treated as adequate.
    lower_bound_prevalence = below_observed / n_total,
    lower_bound_prevalence_pct = pct(below_observed / n_total),

    # Extreme upper bound:
    # every missing Kotelchuck record is treated as below adequate.
    upper_bound_prevalence = (below_observed + n_missing) / n_total,
    upper_bound_prevalence_pct = pct(
      (below_observed + n_missing) / n_total
    )
  )
}

bounds_overall <- calc_bounds(sinasc_full)

# CORRECTED: do not use "." inside the native |> pipe.
region_split <- split(
  sinasc_full,
  sinasc_full[[region_full_col]],
  drop = TRUE
)

bounds_region <- purrr::imap_dfr(
  region_split,
  function(df, nm) {
    calc_bounds(
      df,
      group_label = "Region",
      group_value = nm
    )
  }
)

year_split <- split(
  sinasc_full,
  sinasc_full[[year_full_col]],
  drop = TRUE
)

bounds_year <- purrr::imap_dfr(
  year_split,
  function(df, nm) {
    calc_bounds(
      df,
      group_label = "Year",
      group_value = nm
    )
  }
)

# ============================================================
# 4) Compact manuscript comparison table
# ============================================================
hc1_m2 <- hc_m2_compare |>
  dplyr::filter(se_type == "HC1") |>
  dplyr::rename(
    pr_hc1 = pr,
    conf_low_hc1 = conf_low,
    conf_high_hc1 = conf_high,
    p_value_hc1 = p_value
  ) |>
  dplyr::select(
    model, race,
    pr_hc1, conf_low_hc1, conf_high_hc1, p_value_hc1
  )

hc3_m2 <- hc_m2_compare |>
  dplyr::filter(se_type == "HC3") |>
  dplyr::rename(
    pr_hc3 = pr,
    conf_low_hc3 = conf_low,
    conf_high_hc3 = conf_high,
    p_value_hc3 = p_value
  ) |>
  dplyr::select(
    model, race,
    pr_hc3, conf_low_hc3, conf_high_hc3, p_value_hc3
  )

manuscript_summary <- cluster_compare_m2 |>
  dplyr::left_join(hc1_m2, by = c("model", "race")) |>
  dplyr::left_join(hc3_m2, by = c("model", "race"))

# ============================================================
# 5) Export
# ============================================================
readr::write_csv(
  hc_all,
  file.path(out_dir, "sensitivity_hc_all_coefficients.csv")
)
readr::write_csv(
  hc_race,
  file.path(out_dir, "sensitivity_hc_race_terms.csv")
)
readr::write_csv(
  hc_m2_compare,
  file.path(out_dir, "sensitivity_m2_hc0_hc1_hc3.csv")
)
readr::write_csv(
  cluster_all,
  file.path(out_dir, "sensitivity_clustered_all_coefficients.csv")
)
readr::write_csv(
  cluster_race,
  file.path(out_dir, "sensitivity_clustered_race_terms.csv")
)
readr::write_csv(
  cluster_compare_m2,
  file.path(out_dir, "sensitivity_clustered_vs_primary_m2.csv")
)
readr::write_csv(
  bounds_overall,
  file.path(out_dir, "sensitivity_missing_bounds_overall.csv")
)
readr::write_csv(
  bounds_region,
  file.path(out_dir, "sensitivity_missing_bounds_by_region.csv")
)
readr::write_csv(
  bounds_year,
  file.path(out_dir, "sensitivity_missing_bounds_by_year.csv")
)
readr::write_csv(
  manuscript_summary,
  file.path(out_dir, "sensitivity_manuscript_summary.csv")
)

writexl::write_xlsx(
  list(
    HC_all_coefficients = hc_all,
    HC_race_terms = hc_race,
    M2_HC0_HC1_HC3 = hc_m2_compare,
    Clustered_all_coefficients = cluster_all,
    Clustered_race_terms = cluster_race,
    M2_clustered_vs_primary = cluster_compare_m2,
    Missing_bounds_overall = bounds_overall,
    Missing_bounds_by_region = bounds_region,
    Missing_bounds_by_year = bounds_year,
    Manuscript_summary = manuscript_summary
  ),
  path = file.path(
    out_dir,
    "Sensitivity_Analyses_SINASC_Q00_Q07.xlsx"
  )
)

# ============================================================
# 6) Console summary
# ============================================================
cat("\n============================================================\n")
cat("SENSITIVITY ANALYSIS COMPLETED\n")
cat("============================================================\n\n")

cat("Analysis sample N:", nrow(analysis_cc), "\n")
cat("Full Q00-Q07 cohort N:", nrow(sinasc_full), "\n")
cat("Municipality cluster column:", cluster_col, "\n")
cat(
  "Number of municipality clusters:",
  dplyr::n_distinct(cluster_var, na.rm = TRUE),
  "\n\n"
)

cat("M2: HC0 vs municipality-clustered HC1\n")
print(cluster_compare_m2, n = Inf)

cat("\nM2: HC0 vs HC1 vs HC3\n")
print(hc_m2_compare, n = Inf)

cat("\nOverall missing-outcome bounds\n")
print(bounds_overall, n = Inf)

cat("\nRegional missing-outcome bounds\n")
print(bounds_region, n = Inf)

cat(
  "\nWorkbook saved to:\n",
  file.path(out_dir, "Sensitivity_Analyses_SINASC_Q00_Q07.xlsx"),
  "\n"
)

cat("\nNo primary manuscript result has been overwritten.\n")
