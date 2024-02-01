library(tidyverse)
library(janitor)
library(gtsummary)

rd <- tidytuesdayR::tt_load(2023, week = 43)
prp_raw <-  rd$patient_risk_profiles

prp <- prp_raw %>%
  clean_names() %>%
  pivot_longer(!c(person_id, names(.)[21:100]), names_to = "age_group",
               values_to = "age_value") %>%
  filter(age_value != 0) %>%
  pivot_longer(!c(person_id,names(.[4:83])), names_to = "sex",
               values_to = "sex_values") %>%
  filter(sex_values != 0) %>%
  select(person_id, sex)

prp_raw %>%
  tbl_summary()
