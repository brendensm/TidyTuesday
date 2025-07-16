library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

source("code-for-posting/socials.R")

tuesdata <- tt_load(2025, 26)

list2env(tuesdata, envir = .GlobalEnv)

readme(tuesdata)

gasw <- weekly_gas_prices


sysfonts::font_add("Futura", "/System/Library/Fonts/Supplemental/Futura.ttc")
showtext_auto()
caption <- social_caption("brendensm", "beesm", "brendenmsmith", icon_color =  "#340042", text_color = "grey15", family = "Futura")


ann_text <- data.frame(
  year = 2011,
  avg_dif = .595,
  label = "2022 peak\nof $0.63",
  grade = "midgrade" 
)

arrow_data <- data.frame(
  x = 2016,      
  y = .58,       
  xend = 2021,  
  yend = .6,   
  grade = "midgrade"   
)

gasw |> 
  filter(fuel == "gasoline" & formulation != "all" & grade != "all" 
  ) |> pivot_wider(
    values_from = price,
    names_from = formulation
  ) |> drop_na() |> 
  mutate(difference = reformulated - conventional,
         year = year(date)
         ) |> 
  group_by(year, grade) |> 
  summarise(avg_dif = mean(difference)) |> 
  mutate(grade = fct_relevel(grade, c("regular", "midgrade", "premium"))) |> 
  ggplot(aes(year, avg_dif, fill = grade)) +
  geom_col() +
  facet_grid(~reorder(grade, grade)) +
  geom_text(data = ann_text, aes(label = label, family = "Futura")) +
  geom_curve(data = arrow_data,
             aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")),
             curvature = 0.2,
             color = "grey20") +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_viridis_d() +
  labs(subtitle = "By average annual price, reformulated (cleaner burning) gasoline has become increasingly\nmore expensive. The disparity is greatest in midgrade fuel.",
       title = "The average price difference between conventional and reformulated\ngasoline has increased across fuel grades",
       caption = paste0("TidyTuesday Week 26, 2025 | ", caption)) +
  theme_minimal(base_size = 16, base_family = "Futura") +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption = element_textbox_simple(color = "gray15", halign = -.25, margin = margin(t = 10)),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 16),
        plot.title = element_text(size = 22))


