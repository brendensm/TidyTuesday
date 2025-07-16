library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2024, week = 36)

survey <- tuesdata$stackoverflow_survey_single_response
