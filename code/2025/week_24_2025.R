
library(tidyverse)
library(tidytuesdayR)
                

tuesdata <- tt_load(2025, 24)

list2env(tuesdata, envir = .GlobalEnv)

readme(tuesdata)
                
                
