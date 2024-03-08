library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load(2023, week = 47)
rladies <- tuesdata$rladies_chapters


sub_text <- glue::glue(
  "The frequencies of ",
  '<span style = "color:#51127c">**in-person**</span> ',
  "and ",
  '<span style = "color:#b73779">**virtual**</span> ', 
  "events."
)

logo <- png::readPNG("images/rladies-trans.png")

rladies %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(year, fill = location)) +
  geom_bar(position = position_dodge(preserve = "single")) +
  labs(
    title = "R-Ladies Events by Year and Location",
    x = "",
    y = "",
    subtitle = sub_text,
    caption = "Brenden Smith | Data: R-Lades Meetup Archive | TidyTuesday Week 47"  ) +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  ylim(0, 902) +
  theme(plot.caption = element_text(color = "white"),
        legend.position = "none",
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.subtitle = element_markdown(color = "white", family = "Georgia"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", face = "bold", size = 24),
        plot.title.position = "plot",
        panel.grid.major = element_line(color = "grey15"),
        plot.background = element_rect(fill = "black"),
        panel.grid.minor = element_line(color = "black")) +
  scale_fill_viridis_d(option = "magma", begin = .2, end = .5) +
  annotation_custom(grob = grid::rasterGrob(logo, interpolate = TRUE, 
                    width=unit(2.5,'cm'),
                    hjust = -3.05, vjust=-1.3)) 
  

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


