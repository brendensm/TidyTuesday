library(cranlogs)
library(tidyverse)

cdc <- cran_downloads(packages="CDCPLACES", from = "2023-12-18", to = Sys.Date()-1)

cdc |> 
  group_by(week = floor_date(date, "week")) |> 
  summarise(
    downloads = sum(count)
  ) |> 
  filter(week < "2024-12-14") |> 
  ggplot(aes(week, downloads)) +
  geom_area(fill = "#287D8EFF") +
  geom_vline(xintercept = as.Date("2024-09-18"), size = 1, linetype = "longdash")+
  annotate("text", x = as.Date("2024-07-05"), y = 5100, label = "Release of\nversion 1.1.8", 
           color = "black", size = 5, hjust = 0, family = "Futura") +
  geom_curve(aes(x = as.Date("2024-08-10"), y = 4600, xend = as.Date("2024-09-15"), yend = 4000), 
             arrow = arrow(length = unit(0.2, "cm")), color = "black", size = .65, curvature = 0.2) +
  theme_minimal(base_size = 20, base_family = "Futura") +
  labs(title = "{CDCPLACES} Weekly Downloads", y = "", x="",
       caption = "Data from the {cranlogs} package") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank()
  )#
