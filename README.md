# Prenatal care utilization by maternal race/color in SINASC, Brazil, 2019–2023

Reproducibility repository for the study:

**Maternal Race/Color and Below-Adequate Prenatal Care Utilization among Live Births with Congenital Nervous System Anomalies in Brazil, 2019–2023**

## Study design

Population-based cross-sectional analysis of Brazilian Live Birth Information System (SINASC) public-use data, 2019–2023.

The analytical population includes live births with at least one ICD-10 Q00–Q07 congenital nervous system anomaly code.

## Data

SINASC public-use datasets are available from OpenDataSUS, Brazilian Ministry of Health.

Individual-level SINASC files are not redistributed in this repository.

## Software

- R 4.6.1
- Package versions are recorded in `renv.lock`
- Session information is available in `sessionInfo.txt`

## Reproduction

Run the scripts in numerical order:

1. `01_setup.R`
2. `02_import_recode.R`
3. `03_audit_sample.R`
4. `04_descriptives.R`
5. `05_models.R`
6. `06_temporal_analysis.R`
7. `07_missingness.R`
8. `08_export_results.R`
9. `10_sensitivity_analysis_v2.R`
10. `99_session_info.R`

Restore package versions with:

```r
renv::restore()
```

## Primary analytical sample

Common complete-case sample: N = 11,277.

## Sensitivity analyses

Sensitivity analyses include:

- HC0, HC1 and HC3 variance estimators;
- municipality-clustered robust standard errors;
- extreme-case prevalence bounds for unclassifiable Kotelchuck records.

## Author

Vinicius de Almeida Lucchesi  
Independent Researcher, Osasco, São Paulo, Brazil
