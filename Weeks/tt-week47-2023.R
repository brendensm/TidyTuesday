library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load(2023, week = 47)
rladies <- tuesdata$rladies_chapters

rladies %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(year, fill = location)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(
    title = "R-Ladies Events by Year and Location",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  theme(legend.position = "bottom",
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "black")) +
  scale_fill_viridis_d(option = "magma", begin = .2, end = .5)

rladies %>%
  filter(chapter == "rladies-")


year_counts <- rladies %>%
  group_by(year) %>%
  count(chapter) 

year_counts %>%
  group_by(year) %>% top_n(5,n) %>% 
  filter(year > 2016) %>%
  ggplot(aes(n, chapter)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year)


