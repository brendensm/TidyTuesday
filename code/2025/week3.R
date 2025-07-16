library(tidytuesdayR)
library(tidyverse)
library(NatParksPalettes)
library(ggtext)
library(bhelpers)

data <- tt_load(2025, 3)

exped <- data$exped_tidy
peaks <- data$peaks_tidy

common_peaks <- exped |> count(PEAKID, sort=TRUE) |> top_n(5)

sysfonts::font_add("Rockwell", "/System/Library/Fonts/Supplemental/Rockwell.ttc")

caption <- social_caption("brendensm", "beesm", "brendenmsmith",
                                 icon_color = "gray25", text_color = "gray25",
                                 family = "Rockwell")


p2 <- peaks |> select(PKNAME, PEAKID, HEIGHTM) |> arrange(-HEIGHTM) |>
  filter(PEAKID %in% common_peaks$PEAKID) |> 
  left_join(common_peaks, by = "PEAKID")

main <- p2 |> 
  arrange(-n) |> 
  mutate(id = row_number(),
         PKNAME = factor(PKNAME, levels = PKNAME)) |>
  group_by(PKNAME) |> 
  reframe(x = id + c(-0.6, 0.6, 0, -0.6),
          y = c(0, 0, HEIGHTM, 0)) |>
  ggplot(aes(x, y, fill = PKNAME)) +
  geom_polygon() +
  geom_text(data = p2 |> arrange(-n), 
            aes(x = seq_along(PKNAME), y = 650, label = paste(n, "climbs")),
            color = 'white', size = 5, #fontface = 'bold', 
            family = "Rockwell") +
    geom_text(data = p2|> arrange(-n), 
            aes(x = seq_along(PKNAME), y = 1500, label = paste0(HEIGHTM, "m")),
            color = 'white', size = 6, #fontface = 'bold', 
            family = "Rockwell") +
  geom_text(data = p2|> arrange(-n), 
            aes(x = seq_along(PKNAME), y = HEIGHTM +250, label = PKNAME),
            nudge_y = 6, #fontface = 'bold', 
            size = 5, color = 'gray25', family = "Rockwell") + 
  scale_fill_manual(values = natparks.pals("Torres")) +
  labs(title = "Heights of the Most Commonly Climbed Himalayan Peaks (2020–2024)",
       caption = paste0("TidyTuesday Week 3, 2025 | ", caption)
       ) +
  
    theme_void(base_size = 16, base_family = "Rockwell")  +
  theme(legend.position = "none",
        plot.caption = element_textbox_simple(color = "gray25"),
         plot.title = element_text(face = "bold", color = "gray25", size = 20),
       
        panel.border = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "#F9ECE8", color = "#F9ECE8"))

# ggsave("viz output/week3-2025.png", main, width = 1200, height = 600, units = "px")

