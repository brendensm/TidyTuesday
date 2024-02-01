library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load(2023, week = 27)

historical_markers <- tuesdata$`historical_markers`
no_markers <- tuesdata$`no_markers`

skim(historical_markers)

historical_markers %>%
  group_by(state_or_prov) %>%
  count(sort = T) %>%
  ungroup() %>%
  slice_max(n = 10, order_by = n) %>%
  ggplot(aes(fct_reorder(state_or_prov, n), n)) +
  geom_col() +
  coord_flip()

  
usmap::plot_usmap("counties", include = "MI")
counties <- map_data("county")
mi_county <- subset(counties, region == "michigan")

hist_mi <- historical_markers %>%
  filter(state_or_prov == "Michigan") %>%
  mutate(county_name = str_remove_all(county_or_parish, "County"))

hist_mi %>%
  group_by(county_name) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n)*100) %>%
  view()


mi_map <- mi_county %>%
  ggplot(aes(long, lat, 
             group = group))+
  geom_polygon(colour = alpha("gray30", 1/2), fill = "black") +
  geom_point(data = hist_mi, aes(longitude_minus_w, latitude_minus_s),
             inherit.aes = FALSE, size = .05, color = "white") +
  theme_void() +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill = "gray30"))

hist_mi %>%
  group_by(county_name) %>%
  count(sort = T) %>%
  ungroup() %>%
  slice_max(n = 10, order_by = n) %>%
  ggplot(aes(fct_reorder(county_name, n), n)) +
  geom_col(fill = "white") +
  geom_text(aes(label = n),
            hjust = 1.25, color = "black", size = 4) +
  coord_flip() +

  labs(x = "", y = "") +
  theme_void() +
  theme(panel.background = element_rect(fill = "gray30"),
        plot.background = element_rect(fill = "gray30"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "white"),
        text = element_text(color = "white", size = 14),
        axis.text = element_text(color = "white")) 
  

library(cowplot)
library(ggtext)

mi_county %>%
  ggplot(aes(long, lat, 
             group = group))+
  geom_polygon(colour = alpha("gray40", 1/2), fill = "black") +
  geom_point(data = hist_mi, aes(longitude_minus_w, latitude_minus_s),
             inherit.aes = FALSE, size = .01, color = "white", alpha = .5) +
  labs(title = "Historical Markers in Michigan",
       caption = "Viz: Brenden Smith \nData: www.hmdb.org\nTidyTuesday Week 27, 2023") +
  annotate("text", x = -95, y = 43.75, label = "\n\nMichigan has 1844 historical markers.\nCompared to other states, Michigan ranks\neighth in number of historical markers.\nMost markers are currently in Wayne,\nOakland, and Washtenaw Counties. \n\nThree Counties currently have no historical\nmarkers: Ontonagon, Missauki, and Gladwin.", 
                    colour = "white",
           family = "Georgia", hjust = "left", lineheight = 1, size = 4.5) +
  theme_map() +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill = "gray30", color = "grey30"),
        title = element_text(family = "Georgia", size = 22, face = "bold",
                             color = "white"),
        plot.background = element_rect(fill = "grey30", color = "grey30"),
        plot.title = element_text(margin = margin(10, 0, 0, 0)),
        plot.caption = element_text(size = 8, face = "plain"),
        text = element_text(size = 12, family = "Georgia"),
        plot.subtitle = element_text(size = 12, family = "Georgia", face = "plain"),
        panel.border = element_blank())
