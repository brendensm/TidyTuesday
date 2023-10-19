library(tidyverse)
library(sjPlot)

tuesdata <- tidytuesdayR::tt_load(2023, week = 30)

scurvy <- tuesdata$scurvy

scurvy_clean <- scurvy %>%
  mutate(gum_lik = case_when(
    gum_rot_d6 == "0_none" ~ 1,
    gum_rot_d6 == "1_mild" ~ 2,
    gum_rot_d6 == "2_moderate" ~ 3,
    gum_rot_d6 == "3_severe" ~ 4,
  ))
    
            



plot_likert(scurvy_clean[9],
            legend.labels = c("Severe", "Moderate",
                              "Mild", "None"),
            grid.range = c(.4, .4),
            reverse.scale = T)
