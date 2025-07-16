library(tidyverse)
library(tidytuesdayR)
library(sf)
library(ozmaps)
library(cowplot)
library(png)
library(ggimage)
library("ggpubr")

tuesdata <- tt_load(2023, week = 10)
numbats <- tuesdata$numbats #%>%
  

sf_oz <- ozmap("states")

numbats_sf <- numbats %>%
  filter(!is.na(decimalLatitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                 crs = 4283, agr = "constant") 

bat_in_state <- st_join(numbats_sf, sf_oz, join = st_within)

count_by_state <- bat_in_state %>%
  group_by(NAME, year) %>% count() %>%
  st_drop_geometry()

count_state_yr <- bat_in_state %>%
  group_by(NAME, year) %>% count() %>%
  st_drop_geometry()

count_state_overall <- bat_in_state %>%
  group_by(NAME) %>% count() %>%
  st_drop_geometry()

count_state_overall_geo <- left_join(sf_oz, count_state_overall, by = "NAME")
  
sf_oz_count <- left_join(sf_oz, count_by_state, by = "NAME")

sf_oz_count %>% filter(!is.na(year)) %>% filter(year >= 1954) %>%
ggplot(aes(fill = n)) + 
  geom_sf() + 
  scale_fill_gradientn(colors = c("#F2F5C6", "#62BDBB", "#274294"),  na.value = "grey60",
                       limits = c(0, 277)) +
  theme_void() +
  facet_wrap(~year, nrow = 3) +
  theme(legend.position = "top", 
        legend.key.width = unit(2.5, "cm"),
        text = element_text(size = 16))


centroid <- st_centroid(sf_oz) %>%
  mutate(cen_lon = sf::st_coordinates(.)[,1],
         cen_lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

names_oz_count <- left_join(sf_oz_count, centroid, by = "NAME")

count_state_overall_geo_centroid <- left_join(count_state_overall_geo, centroid, by = "NAME") %>%
  filter(NAME != "Other Territories")

map <- count_state_overall_geo_centroid %>%
  ggplot(aes(fill = n)) + 
  geom_sf() + 
  scale_fill_gradientn(colors = c("#F2F5C6", "#62BDBB", "#274294"),  na.value = "grey60") +
  theme_void() +
  ggrepel::geom_text_repel(data = count_state_overall_geo_centroid, 
                           aes(x = cen_lon, y = cen_lat), 
                           label = count_state_overall_geo_centroid$NAME,
                           size = 3.5,
                           col = "black",
                           nudge_x = c(15, -10, 15, -15, -15, 10, 0, 20, -10),
                           nudge_y = c(2, -5, 3, -10, -11, -3, 10, -5, 10)) +
  theme(legend.position = "left", 
        legend.key.height = unit(1, "cm"),
        legend.margin = margin(l = 4),
        text = element_text(size = 12)) +
  annotation_custom(grob = a)

count_state_yr %>%
  filter(year >= 2005) %>%
  ggplot(aes(x = year, y = n, color = NAME)) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(values =  c("#F2F5C6", "#62BDBB", "#274294")) +
  theme(legend.position = "bottom")

numbats %>%
  group_by(month, hour) %>%
  count(hour = as.character(hour)) %>%
  ggplot(aes(x = month, y = n, fill = hour)) + 
  geom_bar(stat = "identity")

numbats %>%
  filter(year >2005)%>%
  ggplot(aes(hour, fill = month)) +
  geom_histogram() +
  facet_wrap(~year)

mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
         "Sep", "Oct", "Nov", "Dec")

heat <- numbats %>%
  group_by(year, month) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n/sum(n)) %>%
  filter(year>=2006) %>%
  ggplot(aes(year, factor(month, levels = mon))) +
  geom_tile(aes(fill = pct), colour = "white") +
  scale_fill_gradientn(colors = c("#F2F5C6", "#62BDBB", "#274294"),  na.value = "grey60") +
  #scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey60") +
  theme_minimal() #+
 # theme(plot.background = element_rect(fill = "#F8F0E3"),
      #  panel.background = element_rect(fill = "#F8F0E3"))

text <- ggplot() + 
  annotate("text",
           hjust = 1,
           vjust = -8.5,
           x = 10,
           y = 10,
           size = 10,
           label = "Numbats in Australia") +
  theme_void() 

img <- readPNG("numbat.png")


top_left <- ggdraw(
  ggplot() + 
    annotate("text",
             hjust = 1.45,
             vjust = 0,
             x = 1,
             y = 1,
             size = 10,
             label = "Numbats in \nAustralia") +
    theme_void() 
) +
  draw_image(img, width = .5, height = .5, hjust = -.75, vjust = -.2)


 top <- ggarrange(top_left, heat, ncol = 2)
 
plot_grid(text, numbat_trans, map, heat, nrow = 2, ncol = 2)

ggarrange(heat, map, ncol = 2)


ggarrange(
  top,
  map,
  nrow = 2
) 


count_state_overall_geo_centroid %>%
  ggplot(aes(fill = n)) + 
  geom_sf() + 
  scale_fill_gradientn(colors = c("#F2F5C6", "#62BDBB", "#274294"),  na.value = "grey60") +
  theme_void() +
  ggrepel::geom_text_repel(data = count_state_overall_geo_centroid, 
                           aes(x = cen_lon, y = cen_lat), 
                           family = "PT Serif",
                           label = count_state_overall_geo_centroid$NAME,
                           size = 4,
                           col = "black",
                           nudge_x = c(15, -10, 15, -15, -18, 10, 0, 22),
                           nudge_y = c(1, -5, -2.5, -10, 11, -3, 10, -5)) +
  labs(title = " Numbats of Australia",
       subtitle = "  The number of numbat sightings in each Australian state from 1856-2023. 
  Sightings are most frequent in the south, with the highest counts reported 
  in Western Australia, South Australia, and New South Wales.",
       caption = "Viz: Brenden Smith \nData: TidyTuesday Week 10, 2023 \nPhoto: Encyclopedia Britannica, 2010 ",
       fill = "Count") +
  theme(legend.position = "left", 
        legend.key.height = unit(1, "cm"),
        legend.title.align = -1,
        legend.margin = margin(l = 10, t = 10),
        text = element_text(size = 12, family = "PT Serif"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        plot.title = element_text(size = 22, face = "bold"),
        plot.background = element_rect(fill = "#F8F0E3", color = "#F8F0E3")
        ) +
  annotation_custom(grob = grid::rasterGrob(img, interpolate = TRUE, 
                                            width=unit(5.25,'cm'),
                                            x = unit(1,"npc"), y = unit(1,"npc"),
                                            hjust = 1, vjust=1))

numbats %>%
  group_by(year, month) %>%
  count() %>%
  ungroup() %>%
  filter(year>=2006) %>%
  ggplot(aes(year, factor(month, levels = mon))) +
  geom_tile(aes(fill = n), colour = "black") +
  scale_fill_gradientn(colors = c("#F2F5C6", "#62BDBB", "#274294"),  na.value = "grey60") +
  labs(title = "Numbats Prefer the End of the Year",
       subtitle = "Most sightings occured in the warmer months in Australia,with notable spikes\nin November, December, and January.",
       x = "Year", y = "Month", fill = "Count",
       caption = "Viz: Brenden Smith\nData: TidyTuesday Week 10, 2023") +
  theme_minimal() +
  theme(legend.position = "left", 
        legend.key.height = unit(1, "cm"),
        legend.margin = margin(t = 10),
        text = element_text(size = 12, family = "PT Serif"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        plot.title = element_text(size = 22, face = "bold"),
        plot.background = element_rect(fill = "#F8F0E3"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey")
  )



