library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2024-01-23')
eng_ed <- tuesdata$english_education

eng_ed |> 
  group_by(income_flag, rgn11nm) |> 
  summarise(mean_ed_score = mean(education_score)) |> 
  filter(income_flag != "Cities") |> 
  drop_na() |> 
  ggplot(aes(mean_ed_score, reorder(rgn11nm, mean_ed_score))) +
  geom_point(aes(color = income_flag), size = 4, alpha = .85) +
  geom_point(shape = 1,size = 4,colour = "grey50") +
  geom_vline(xintercept = 0, color = "grey70") +
  annotate('text', x = -1.8, y = 8.5, label = "Higher income deprivation", color = "grey50") +
  annotate('text', x = 1.45, y = 8.5, label = "Mid", color = "grey50") +
  annotate('text', x = 5.4, y = 8.5, label = "Lower", color = "grey50") +
  annotate('curve', x = -1.8, y = 8.325,
           xend = -1.5, yend = 8, color = "grey50",
           curvature = 0.35,
           arrow = arrow(length = unit(0.15, "cm"))) +
  annotate('curve', x = 1.4, y = 8.325,
           xend = 1.55, yend = 8.1, color = "grey50",
           curvature = 0.2,
           arrow = arrow(length = unit(0.15, "cm"))) +
  annotate('curve', x = 5.4, y = 8.325,
           xend = 5.2, yend = 8, color = "grey50",
           curvature = -0.35,
           arrow = arrow(length = unit(0.15, "cm"))) +
  xlim(-4, 6) +
 # ylim(0, 12) +
  labs(x = "", y = "",
       title = "Average educational attainment score for towns, by region and income \ndeprivation level, England",
       subtitle = "Across all income deprivation levels, the North West and Northeast have consistently higher\neducation attainment.",
       caption = "Brenden Smith | TidyTuesday Week 4, 2024 | Data: UK Office for National Statistics") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold",
                                  color = "grey30"),
        plot.subtitle = element_text(color = "grey30")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 14)) +
  scale_color_manual(values = c("#871A5B", "#206095", "#D3D3D3"))

