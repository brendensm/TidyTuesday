library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(grid)
library(gridExtra)

tuesdata <- tidytuesdayR::tt_load(2024, 9)

events <- tuesdata$events
births <- tuesdata$births
deaths <- tuesdata$deaths

bg <- births %>% 
  count(year_birth, sort = TRUE) %>% 
  filter(year_birth > 1900) %>% 
  ggplot(aes(year_birth, n)) +
  geom_col(fill = "turquoise4") +
  scale_x_continuous(limits = c(1900, 2025), breaks = seq(1900, 2025, by = 20)) +
  labs(x = "", y = "") +
  theme_minimal(base_size = 14, 
                base_family = "Helvetica") +
  theme(panel.grid = element_line(color = "grey75"),
        plot.margin = margin(b = -15))

dg <- deaths %>% 
  count(year_death, sort = TRUE) %>% 
  filter(year_death > 1900) %>% 
  ggplot(aes(year_death, n)) +
  geom_col(fill = "tomato2") +
  scale_x_continuous(limits = c(1900, 2025), breaks = seq(1900, 2025, by = 20)) +
  ylim(0, 8) +
  labs(x= "", y= "") +
  theme_minimal(base_size = 14, 
                base_family = "Helvetica") +
  theme(panel.grid = element_line(color = "grey75"),
        plot.margin = margin(b = -15))



main <- plot_grid(bg, dg, ncol = 1) 


title <- ggdraw() + 
  draw_label(
    "Number of Leap Year",
    fontface = 'bold',
    fontfamily = "Helvetica",
    x = 0,
    hjust = 0,
    size = 16
  ) +
  draw_label(
    "Births",
    fontface = 'bold',
    fontfamily = "Helvetica",
    x = 0,
    hjust = -3.52,
    color = "turquoise4",
    size = 16
  ) +
  draw_label(
    "and",
    fontface = 'bold',
    fontfamily = "Helvetica",
    x = 0,
    hjust = -7.7,
    size = 16
  ) +
  draw_label(
    "Deaths",
    fontface = 'bold',
    fontfamily = "Helvetica",
    x = 0,
    hjust = -4.7,
    color = "tomato2",
    size = 16
  ) +
  draw_label(
    "According to Wikipedia",
    fontface = 'bold',
    fontfamily = "Helvetica",
    x = 0,
    hjust = -1.79,
    size = 16
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7),
    
  )


sub <-  ggdraw() +
  draw_label(
    "Brenden Smith | TidyTuesday Week 9, 2024",
    fontfamily = "Helvetica",
    x = 0,
    hjust = 0,
    size = 12
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 270)
  )

plot_grid(
  title, main, sub,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1, 0.1)
)
