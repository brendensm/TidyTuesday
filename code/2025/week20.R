library(tidyverse)
library(tidytuesdayR)


d <- tt_load(2025, 20)

water <- d$water_quality
weather <- d$weather  

ww <- water |> left_join(weather, by = "date")


ww |> 
  ggplot(aes(enterococci_cfu_100ml, max_temp_C)) +
  geom_point()


ww |> count(region)
