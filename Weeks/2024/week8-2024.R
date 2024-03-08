library(tidyverse)
library(tidytext)
library(packcircles)
library(viridis)

week8 <- tidytuesdayR::tt_load(2024, 8)
isc_grants <- week8$isc_grants

title_tokens <- isc_grants %>% 
  select(title) %>% 
  unnest_tokens(word, title)

data("stop_words")

title_tokens <- title_tokens %>% 
  anti_join(stop_words)

word_counts <- title_tokens %>% 
  count(word, sort = TRUE) %>% 
  top_n(n = 13, wt = n)

# Generate the layout
packing <- circleProgressiveLayout(word_counts$n, sizetype='area')
packing$radius <- 0.98*packing$radius
word_counts <- cbind(word_counts, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis(option = "E") +
  geom_text(data = word_counts, aes(x, y, label = word), color="black", size=4, family = "Futura") +
  theme_void(base_family = "Futura", base_size = 14) + 
  coord_equal() +
  labs(title = "Most Common Words in R Consortium Infrastructure\nSteering Committee Grant Project Titles",
       caption = "Brenden Smith | TidyTuesday Week 8, 2024") +
  theme(legend.position = "none",
    plot.title = element_text(size = 16),
    plot.title.position = "plot",
    plot.margin = margin(b = 5))
