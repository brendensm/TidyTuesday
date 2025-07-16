library(tidyverse)
library(sjPlot)

tuesdata <- tidytuesdayR::tt_load(2023, week = 30)

scurvy <- tuesdata$scurvy

scurvy_clean <- scurvy %>%
  mutate(gum_rot = case_when(
    gum_rot_d6 == "0_none" ~ 1,
    gum_rot_d6 == "1_mild" ~ 2,
    gum_rot_d6 == "2_moderate" ~ 3,
    gum_rot_d6 == "3_severe" ~ 4,
  ),
  skin_sores = case_when(
    skin_sores_d6 == "0_none" ~ 1,
    skin_sores_d6 == "1_mild" ~ 2,
    skin_sores_d6 == "2_moderate" ~ 3,
    skin_sores_d6 == "3_severe" ~ 4,
  ),
  weakness_of_the_knees = case_when(
    weakness_of_the_knees_d6 == "0_none" ~ 1,
    weakness_of_the_knees_d6 == "1_mild" ~ 2,
    weakness_of_the_knees_d6 == "2_moderate" ~ 3,
    weakness_of_the_knees_d6 == "3_severe" ~ 4,
  ),
  lassitude = case_when(
    lassitude_d6 == "0_none" ~ 1,
    lassitude_d6 == "1_mild" ~ 2,
    lassitude_d6 == "2_moderate" ~ 3,
    lassitude_d6 == "3_severe" ~ 4,
  ),
  fit_for_duty = case_when(
    fit_for_duty_d6 == "0_no" ~ 1,
    fit_for_duty_d6 == "1_yes" ~ 2,
  )) %>%
  select(-ends_with("d6"))
    
            
scurv_long <- scurvy_clean %>%
  pivot_longer(!c(study_id, treatment, dosing_regimen_for_scurvy, fit_for_duty),
               names_to = "symptom", values_to = "severity") %>%
  mutate(severity = factor(severity, labels = c("none", "mild", "moderate", "severe")))


plot_likert(scurvy_clean[4:7],
            legend.labels = c("Severe", "Moderate",
                              "Mild", "None"),
            grid.range = c(.4, .4),
            reverse.scale = T,
            groups = c(1, 1, 2, 2, 3, 3, 4, 4 ,5 ,5, 6, 6))


scurv_long %>%
  group_by(treatment, symptom) %>%
  count(severity) %>%
  ggplot(aes(symptom, n, fill = severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~treatment) +
  coord_flip()
