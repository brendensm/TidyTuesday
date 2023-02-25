library(tidyverse)
library(tidytuesdayR)
library(ggtext)

tuesdata <- tt_load(2023, week = 8)

bob_ross <- tuesdata$bob_ross


colors_sub <- bob_ross %>%
              separate_rows(colors, color_hex, sep = ", ") %>%
              select(colors, color_hex) %>%
              mutate(colors = str_remove_all(colors, "\\[|\\]|\\'|\\\\r|\\\\n"),
                     color_hex = str_remove_all(color_hex, "\\[|\\]|\\'|\\\\r|\\\\n"))

hex_df <- unique(colors_sub)

paint_count <- colors_sub %>%
                  group_by(colors) %>%
                  summarise( n = n()) %>%
                  slice_max(n = 15, order_by = n) %>%
                  inner_join(., hex_df, by = "colors")

top_palette <- paint_count$color_hex
  
paint_count %>%
  ggplot(aes(reorder(colors, n), n)) +
  geom_col(fill = paint_count$color_hex) +
  coord_flip() +
  labs(title = "Most Common Paint Colors Used by Bob Ross",
       subtitle = "Frequencies of the top 15 colors used in all 31 seasons of 'The Joy of Painting'.",
       x = "Colors", y = "Times used",
       caption = "Visualization by Brenden Smith\nData from TidyTuesday week 8 2023") +
  theme_grey() +
  theme(plot.title.position = "plot", plot.background = element_rect("#D2D2D2"),
        panel.background = element_rect("#D2D2D2"),
        axis.text = element_text(color = "#000000"),
        plot.title = element_text(face = "bold", size = 14))

