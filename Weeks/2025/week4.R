library(tidytuesdayR)
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(ggtext)

tt <- tt_load(2025, 4)

wi22 <- tt$water_insecurity_2022
wi23 <- tt$water_insecurity_2023

sysfonts::font_add("Futura", "/System/Library/Fonts/Supplemental/Futura.ttc")
showtext::showtext_auto()

caption <- social_caption("brendensm", "beesm", "brendenmsmith",
                          icon_color = "#1D0C2A", text_color = "black",
                          family = "Futura")

counties <- tigris::counties(year = 2023, cb =TRUE) |> 
  filter(STUSPS %in% c("NM", "AZ", "UT")) |> 
  janitor::clean_names()


wi23_geo <- wi23 |> left_join(counties, by = "geoid") |> st_as_sf()

# Define the variable names: total population and plumbing lacking households
# B01003_001: total population, B25049_004: households lacking plumbing
vars <- c("B01003_001", "B25049_004")

# Pull data for 2023 and 2022 
AIANA <- get_acs(
  geography = 'american indian area/alaska native area/hawaiian home land', 
  variables = vars, 
  year = 2023, 
  survey = "acs1",
  geometry = TRUE
) |> tigris::shift_geometry() |> 
  janitor::clean_names() |> 
  # new column variable_long based on the value of the variable column
  mutate(
    variable_long = case_when(
      variable == "B01003_001" ~ "total_pop",
      variable == "B25049_004" ~ "plumbing",
      .default = NA_character_  # In case there are any other variables
    )
  ) |> 
  select(geoid, name, variable_long, estimate, geometry) |> 
  pivot_wider(
    names_from = variable_long,
    values_from = estimate
  ) |> 
  # Add a column for percent of population lacking plumbing in 2023
  mutate(
    percent_lacking_plumbing = (plumbing / total_pop) * 100
  )

#AIANA |> ggplot() +geom_sf()

states <- tigris::states(cb=TRUE, year=2023) |> tigris::shift_geometry()

theme_set(theme_void(base_size = 12, base_family = "Futura"))

s <- states |> 
filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = wi23_geo, aes(fill = percent_lacking_plumbing)) + 
  scale_fill_viridis_c(option="B", label = scales::percent_format(scale = 1), 
                       limits = c(0,6)) +
  theme(legend.position = "top",
        legend.key.width = unit(2, 'cm'),
       # plot.margin = margin(0, 0, 0, 7),
        plot.caption = element_textbox_simple(
          halign = 0.5
          #margin = margin(7, 0, 0, 7))
        )) +
  labs(fill = "", caption = paste0("TidyTuesday Week 4, 2025 | ", caption)) +
  guides(fill = guide_colorbar(title.position = "top"))


legend <- get_plot_component(s, "guide-box", return_all = TRUE)[[4]]

h <- get_plot_component(s, "caption")

s_no_legend <- 
  states |> 
  filter(
    STUSPS %in% c("AZ", "NM", "UT")) |> 
  ggplot()+
  geom_sf() +
  geom_sf(data = wi23_geo, aes(fill = percent_lacking_plumbing)) + 
  annotate("text", x = -900000, y = 220000, label = "Max. \nof 3.9%", family = "Futura") +
  scale_fill_viridis_c(option="B", label = scales::percent_format(scale = 1), limits = c(0, 6)) +
  geom_curve(aes(x = -1050000, y = 170000, xend = -1170000, yend = 40000), 
             arrow = arrow(length = unit(0.2, "cm")), color = "black", size = .65, 
             curvature = 0.2) +
  theme(legend.position = "none")


t <- 
  states |> 
  filter(
    STUSPS %in% c("AZ", "NM", "UT")) |> 
  ggplot()+
  geom_sf() +
  geom_sf(data = filter(tribal_data2022, geoid %in% c("2430")), aes(fill = percent_lacking_plumbing )) +
  annotate("text", x = -900000, y = 220000, label = "Max.\nof 5%", family = "Futura") +
  geom_curve(aes(x = -900000, y = 120000, xend = -1080000, yend = 20000), 
             arrow = arrow(length = unit(0.2, "cm")), color = "black", size = .65, 
             curvature = -0.2) +
  
  scale_fill_viridis_c(option="B",
    label = scales::percent_format(scale = 1), limits = c(0, 6)) +
 # theme_void() +
  theme(legend.position = "none") 



title <- ggdraw() + 
  draw_label(
    "Percentage of Households Lacking Plumbing (2023)",
    fontfamily = "Futura",
    size = 24,
  #  fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


subtitle <- ggdraw() + 
  draw_label(
    "Four out of the five highest percentage U.S. Counties are within the boundaries of the Navajo Nation\nReservation and Off-Reservation Trust Land.",
    fontfamily = "Futura",
    size = 14,
    #  fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )




prow <- plot_grid(s_no_legend, t, align = 'vh',
                 # labels = c("A", "B"),
                  hjust = -1,
                  nrow = 1)
  cowplot::plot_grid(title, subtitle, prow, legend, h, nrow =5, ncol= 1, rel_heights = c(.1, .1, .55, .25, .05))

# 4/5 of the top counties are in the navajo nation

  
  
  library(cowplot)
 # Mobile version 
#  theme_set(theme_void(base_size = 14, base_family = "Futura"))
  
  # Create title with adjusted size for mobile
  title <- ggdraw() + 
    draw_label(
      "Percentage of Households Lacking\nPlumbing (2023)",
      fontfamily = "Futura",
      size = 24,
     # x = .0149,
    #  hjust = 0
    x = 0.095,  # Center point horizontally
    hjust = 0  # Center alignment
    ) 
  
  # Create subtitle with adjusted size and wrapping
  subtitle <- ggdraw() + 
    draw_label(
      "Four out of the five U.S. Counites with the highest \npercentage of households without complete indoor\nplumbing overlap with the boundaries of the Navajo\nNation Reservation and Off-Reservation Trust Land.",
      fontfamily = "Futura",
      size = 16,
      x = 0.095,  # Center point horizontally
      hjust = 0  # Center alignment
      # x = 0,
      # hjust = 0
    )# +
   # theme(plot.margin = margin(0, 0, 0, 7))
  
  # Modify main plots for mobile layout
  s_no_legend <- 
    states |> 
    filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
    ggplot() +
    geom_sf() +
    geom_sf(data = wi23_geo, aes(fill = percent_lacking_plumbing)) + 
    annotate("text", x = -970000, y = 220000, 
             label = "Max.\nof 3.9%", 
             family = "Futura", 
             size = 3.5) +
    scale_fill_viridis_c(
      option = "B", 
      label = scales::percent_format(scale = 1), 
      limits = c(0, 6)
    ) +
    geom_curve(
      aes(x = -1050000, y = 170000, 
          xend = -1170000, yend = 40000), 
      arrow = arrow(length = unit(0.15, "cm")), 
      color = "black", 
      size = .5, 
      curvature = 0.2
    ) +
    theme(legend.position = "none")
  
  t <- 
    states |> 
    filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
    ggplot() +
    geom_sf() +
    geom_sf(
      data = filter(AIANA, geoid %in% c("2430")), 
      aes(fill = percent_lacking_plumbing)
    ) +
    annotate("text", x = -900000, y = 180000, 
             label = "Max.\nof 5%", 
             family = "Futura", 
             size = 3.5) +
    geom_curve(
      aes(x = -900000, y = 120000, 
          xend = -1080000, yend = 20000), 
      arrow = arrow(length = unit(0.15, "cm")), 
      color = "black", 
      size = .5, 
      curvature = -0.2
    ) +
    scale_fill_viridis_c(
      option = "B",
      label = scales::percent_format(scale = 1), 
      limits = c(0, 6)
    ) +
    theme(legend.position = "none")
  
  # Create legend
  legend <- get_plot_component(s, "guide-box", return_all = TRUE)[[4]]
  
  # Create caption
  h <- get_plot_component(s, "caption")
  
  # Combine plots vertically for mobile
  prow <- plot_grid(
    s_no_legend, t, 
    align = 'v',
    nrow = 2,
    rel_heights = c(1, 1)
  )
  
  # Final assembly with adjusted relative heights for mobile
  final_plot <- plot_grid(
    title, subtitle, prow, legend, h, 
    nrow = 5, 
    ncol = 1, 
    rel_heights = c(.1, .08, .72, .05, .05)
  )
  
  # Save with mobile-friendly dimensions
  ggsave(
    "mobile_water_insecurity.png",
    final_plot,
    width = 5,
    height = 10,
    dpi = 300
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  theme_set(theme_void(base_size = 16, base_family = "Futura"))
  
  # Create title with adjusted size for mobile
  title <- ggdraw() + 
    draw_label(
      "Percentage of Households Lacking Plumbing (2023)",
      fontfamily = "Futura",
      size = 24,
      x = 0.5,
      hjust = 0.5
    ) 
  
  # Create subtitle with adjusted size and wrapping
  subtitle <- ggdraw() + 
    draw_label(
      "Four out of the five highest percentage U.S. Counties are within the boundaries\nof the Navajo Nation Reservation and Off-Reservation Trust Land.",
      fontfamily = "Futura",
      size = 16,
      x = 0.5,  # Center point horizontally
      hjust = 0.5  # Center alignment
    )
  
  # Modify main plots for mobile layout
  s_no_legend <- 
    states |> 
    filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
    ggplot() +
    geom_sf() +
    geom_sf(data = wi23_geo, aes(fill = percent_lacking_plumbing)) + 
    annotate("text", x = -900000, y = 220000, 
             label = "Max.\nof 3.9%", 
             family = "Futura", 
             size = 3.5) +
    scale_fill_viridis_c(
      option = "B", 
      label = scales::percent_format(scale = 1), 
      limits = c(0, 6)
    ) +
    geom_curve(
      aes(x = -1050000, y = 170000, 
          xend = -1170000, yend = 40000), 
      arrow = arrow(length = unit(0.15, "cm")), 
      color = "black", 
      size = .5, 
      curvature = 0.2
    ) +
    theme(legend.position = "none")
  
  t <- 
    states |> 
    filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
    ggplot() +
    geom_sf() +
    geom_sf(
      data = filter(tribal_data2023, geoid %in% c("2430")), 
      aes(fill = percent_lacking_plumbing)
    ) +
    annotate("text", x = -900000, y = 220000, 
             label = "Max.\nof 5%", 
             family = "Futura", 
             size = 3.5) +
    geom_curve(
      aes(x = -900000, y = 120000, 
          xend = -1080000, yend = 20000), 
      arrow = arrow(length = unit(0.15, "cm")), 
      color = "black", 
      size = .5, 
      curvature = -0.2
    ) +
    scale_fill_viridis_c(
      option = "B",
      label = scales::percent_format(scale = 1), 
      limits = c(0, 6)
    ) +
    theme(legend.position = "none")
  
  # Create legend
  legend <- get_plot_component(s, "guide-box", return_all = TRUE)[[4]]
  
  # Create centered caption
  caption_text <- paste0("TidyTuesday Week 4, 2025 | ", caption)
  
  
  s <- states |> 
    filter(STUSPS %in% c("AZ", "NM", "UT")) |> 
    ggplot() +
    geom_sf() +
    geom_sf(data = wi23_geo, aes(fill = percent_lacking_plumbing)) + 
    scale_fill_viridis_c(option="B", label = scales::percent_format(scale = 1), 
                         limits = c(0,6)) +
    theme(legend.position = "top",
          legend.key.width = unit(2, 'cm'),
         # plot.margin = margin(0, 0, 0, 7),
          plot.caption = element_textbox_simple(
            halign = 0.5
            #margin = margin(7, 0, 0, 7)
            )) +
    labs(fill = "", caption = paste0("TidyTuesday Week 4, 2025 | ", caption)) +
    guides(fill = guide_colorbar(title.position = "top"))
  
  
  h <- get_plot_component(s, "caption")
  
  # Combine plots vertically for mobile
  prow <- plot_grid(
    s_no_legend, t, 
    align = 'v',
    nrow = 2,
    rel_heights = c(1, 1)
  )
  
  # Final assembly with adjusted relative heights for mobile
  final_plot <- plot_grid(
    title, subtitle, prow, legend, h, 
    nrow = 5, 
    ncol = 1, 
    rel_heights = c(.055, .055, .7, .1, .05)
  )
  