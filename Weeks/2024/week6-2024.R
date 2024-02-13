library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

tuesdata <- tidytuesdayR::tt_load(2024, 6)

heritage <- tuesdata$heritage |> 
  mutate(
    pct_change = `2022`/`2004` - 1
  )

m <- ne_countries(scale = "medium", returnclass = "sf")

countries <- m |> 
 # filter(continent == "Europe") |> 
  filter(admin %in% c("Norway", "Denmark", "Sweden"))

countries |> 
  ggplot() +
  geom_sf(fill = "grey79", color = "white") +
  coord_sf(xlim = c(4,33), ylim = c(54,72), expand = FALSE) +
  theme_void()

heritage_map <- left_join(countries, heritage, by = c("admin" = "country"))

# ball park top left: y = 65 x = 3
# right side: y = 62 x = 26

heritage_map |> 
  ggplot() +
  geom_sf(aes(fill = pct_change), color = "black") +
  coord_sf(xlim = c(-5,40), ylim = c(54,72), expand = FALSE) +
  annotate("text", x = 3, y = 56, label = "150%", size = 9, color = "darkgreen", family = "Courier") +
  annotate("text", x = 3, y = 65, label = "60%", size = 9, color = "#A3C89B", family = "Courier") +
  annotate("text", x = 26, y = 62, label = "15.4%", size = 9, color = "#e9fce9", family = "Courier") +
  annotate('curve', x = 3, y = 55.5,
           xend = 8.35, yend = 55.35, color = "darkgreen",
           curvature = 0.35,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate('curve', x = 2.5, y = 64.45,
           xend = 6.8, yend = 63, color = "#A3C89B",
           curvature = 0.35,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  annotate('curve', x = 26, y = 61.35,
           xend = 19.2, yend = 60, color = "#e9fce9",
           curvature = -0.35,
           arrow = arrow(length = unit(0.2, "cm"))) +
  
  labs(title = "UNESCO World Heritage Sites",
       subtitle = "Percentage change in the number of UNESCO World \nHeritage Sites from 2004 to 2022 in Norway, \nDenmark, and Sweden.",
       caption = "Brenden Smith | TidyTuesday Week 6, 2024") +
  theme_void(base_family = "Courier", base_size = 16) +
  scale_fill_gradient(low = "#e9fce9", high = "darkgreen") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey15"),
        plot.margin = margin(l = 4, r = 4),
        plot.title.position = "plot",
        plot.title = element_text(size = 24, color = "#e9fce9"),
        plot.subtitle = element_text(size = 14, color = "#e9fce9"),
        plot.caption = element_text(size = 12, color = "#e9fce9"))
