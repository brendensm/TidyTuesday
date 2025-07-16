library(tidyverse)
library(tidytuesdayR)
library(ggradar)

tuesdata <- tt_load(2023, week = 9)

languages <- tuesdata$languages
afrisenti <- tuesdata$afrisenti %>%
  left_join(languages, by = "language_iso_code")
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

gg_rad <- afrisenti %>%
  group_by(language, label) %>%
  summarise(n = n()) %>%
  mutate(pct = (prop.table(n))) %>%
  ungroup() %>%
  select(label, language, pct) %>%
  pivot_wider(names_from = language, values_from = pct) 
  
gg_rad$label <- str_to_title(gg_rad$label)

gg_rad %>%
  ggradar(axis.label.size = 3.5,
          font.radar = "Optima",
          grid.max = .75,
          values.radar = c("0%", "50%", "75%"),
          group.point.size = 4,
          fill = TRUE,
          legend.text.size = 12,
          group.colours = c("#de061a", "#d9b42c", "#4fb443"),
          grid.label.size = 5,
          legend.position = "bottom",
          plot.extent.x.sf = 1.25,
          plot.extent.y.sf = 1.15
          ) + 
  labs(title = "African Language Tweet Sentiment Analysis",
       subtitle = "Sentiment comparison from a sample of over 111,000 tweets \nin 14 different African Languages.",
       caption = "Visualization by Brenden Smith\nData from Afrisenti\nTidyTuesday week 9, 2023") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 12), plot.caption = element_text(size = 8),
                    text = element_text(family = "Optima"), plot.title.position = "plot")

