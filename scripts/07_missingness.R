library(tidyverse)
library(here)
dat <- readr::read_rds(here("data","derived","sinasc_recoded.rds"))
audit_missing <- function(df,group_var){df |> group_by({{group_var}}) |> summarise(n_total=n(),kotelchuck_missing_n=sum(is.na(outcome)),kotelchuck_missing_pct=kotelchuck_missing_n/n_total,n_kotel_valid=sum(!is.na(outcome)),race_missing_among_kotel_valid_n=sum(!is.na(outcome)&is.na(race)),race_missing_among_kotel_valid_pct=race_missing_among_kotel_valid_n/n_kotel_valid,n_kotel_race_valid=sum(!is.na(outcome)&!is.na(race)),education_missing_among_kotel_race_valid_n=sum(!is.na(outcome)&!is.na(race)&is.na(education)),education_missing_among_kotel_race_valid_pct=education_missing_among_kotel_race_valid_n/n_kotel_race_valid,.groups="drop")}
missing_year <- audit_missing(dat,year)
missing_region <- audit_missing(dat,region)
write_csv(missing_year,here("outputs","tables","missingness_by_year.csv"))
write_csv(missing_region,here("outputs","tables","missingness_by_region.csv"))
print(missing_year);print(missing_region)
