library(tidyverse)

install.packages("pak")
pak::pak("r4ds/ttmeta")

ttmeta::tt_datasets_metadata |> 
  dplyr::mutate(
    has_country = purrr::map_lgl(variable_details, function(var_details) {
      "country_code" %in% tolower(var_details$variable) ||
        any(stringr::str_detect(tolower(var_details$variable), "country"))
    })
  ) |> 
  dplyr::filter(has_country)

tuesdata <- tidytuesdayR::tt_load(2024, week = 46)

countries <- tuesdata$countries
country_subdivisions <- tuesdata$country_subdivisions
former_countries <- tuesdata$former_countries

sub <- country_subdivisions |> 
  count(alpha_2, sort=TRUE) |> 
  left_join(select(countries, alpha_2, name, official_name, common_name, alpha_3), by = "alpha_2")

countries::quick_map(data = sub[1:200,], plot_col = "n")

# I like this but I want it to look a bit nicer...


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

missing <- c("BES", "UMI", "SSD")

sub2 <- sub |> left_join(select(world, adm0_a3, continent, geometry), by = c("alpha_3" = "adm0_a3")) |> 
  filter(!(alpha_3 %in% missing)) |> 
  sf::st_as_sf()


sub2$bins <- cut(sub2$n, 
                 breaks = c(0, 25, 50, 100, 200, Inf), 
                 labels = c("0-25", "25-50", "50-100", "100-200", ">200"), 
                 include.lowest = TRUE)

library(sf)

sub2 |> 
  ggplot(aes(fill = bins)) +
  geom_sf() +
  scale_fill_viridis_d() +
  theme_void()

margin <- 1  # Adjust this value to increase or decrease the margin

top_countries <- sub2[1:10,] %>%
  rowwise() %>%
  mutate(
    # Get bounding box, convert to list, and add margin
    bbox = st_bbox(geometry),
    bbox_expanded = st_bbox(c(
      xmin = bbox["xmin"] - margin,
      ymin = bbox["ymin"] - margin,
      xmax = bbox["xmax"] + margin,
      ymax = bbox["ymax"] + margin
    )),
    # Convert expanded bbox to an sf object for cropping
    bbox_sf = st_as_sfc(bbox_expanded, crs = st_crs(geometry)),
    geometry = st_crop(geometry, bbox_sf)  # Crop each geometry to its expanded bounding box
  ) %>%
  ungroup()

ten |> 
  ggplot(aes(fill = bins)) +
  geom_sf() +
  scale_fill_viridis_d() +
  theme_void() +
  facet_wrap(~name)

# Filter data for each continent in sub2
sub2_africa <- sub2[sub2$continent == "Africa", ]
sub2_asia <- sub2[sub2$continent == "Asia", ]
sub2_europe <- sub2[sub2$continent == "Europe", ]
sub2_samerica <- sub2[sub2$continent == "South America", ]
sub2_namerica <- sub2[sub2$continent == "North America", ]
sub2_oceania <- sub2[sub2$continent == "Oceania", ]


plot_africa <- ggplot(data = sub2_africa) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Africa") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

# Asia map
plot_asia <- ggplot(data = sub2_asia) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Asia") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

# Europe map
plot_europe <- ggplot(data = sub2_europe) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Europe") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

# Americas map
plot_namerica <- ggplot(data = sub2_namerica) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Americas") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

plot_samerica <- ggplot(data = sub2_samerica) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Americas") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

# Oceania map
plot_oceania <- ggplot(data = sub2_oceania) +
  geom_sf(aes(fill = bins), color = "gray40", size = 0.2) +
  scale_fill_brewer(palette = "YlOrBr", name = "Population") +
  labs(title = "Oceania") +
  theme_minimal() +
  theme(legend.position = "none")+
  coord_sf(default_crs = NULL)

library(cowplot)

# Combine maps into a grid layout
combined_map <- plot_grid(
  plot_africa, plot_asia, plot_europe,     # first row
  plot_namerica, plot_samerica, plot_oceania,             # second row
  ncol = 3                                 # Adjust as desired
)

# Display the combined map
combined_map



install.packages("rgeoboundaries")
library(rgeoboundaries)

europe <- geoboundaries(sub2_europe$alpha_2, adm_lvl = "ADM0")

plot(st_geometry(europe))





