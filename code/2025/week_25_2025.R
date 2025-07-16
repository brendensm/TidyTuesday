library(tidyverse)
library(tidytuesdayR)
#library(gganimate)
                

tuesdata <- tt_load(2025, 25)

list2env(tuesdata, envir = .GlobalEnv)

readme(tuesdata)

cases_year |> names()



  

me <- read_csv("Weeks/2025/measles-cdc.csv")
                



me |> 
  ggplot(aes(year, cases)) +
  geom_col(fill = "grey70") +
  labs(title = "Confirmed measles cases since 2000") +
  theme_classic(base_size = 16) +
  theme(axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold"))


p <- 
  me |> 
  ggplot(aes(year, cases)) +
  geom_col(aes(fill = year == 2025), width = .75) +
  annotate( geom = "text", label = "In 2000, measles was\ndeclared eliminated in\nthe U.S.", 
             x = 1999.9, y = 330,
             hjust = 0) +
    geom_segment(x = 2000, xend=2000, y = 86, yend = 180, color = "grey20") +
    
  annotate(geom="text", label = "1,274 cases\nMeasles spread through\nN.Y. Orthodox Jewish\ncommunities in 2019",
           x = 2017, y = 1200,
           hjust = 1) +  
  geom_segment(x = 2018.5, xend = 2017.25, y = 1273, yend = 1273) +
  annotate(geom="text", label = "1,309\ncases",
           x = 2022.5, y = 1308) +
  geom_segment(x = 2024.5, xend = 2023.5, y = 1308, yend = 1308) +
  
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "#EFC074"), 
                    guide = "none") +
  labs(title = "Confirmed measles cases since 2000",
       caption = "Source: CDC confirmed cases through July 15") +
  theme_classic(base_size = 16) +
  theme(axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold")) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2025),
    labels = function(x) {
      ifelse(x %in% c(2000, 2025), 
             as.character(x), 
             paste0("'", substr(x, 3, 4)))
    }
  ) +
  scale_y_continuous(
    breaks = c(250, 500, 750, 1000, 1250),
    position = "right",
    labels = scales::comma
  ) +
  theme(
    plot.title = element_text(size = 18),
    axis.text.y.right = element_text(),
    axis.ticks.y.right = element_line(linetype = "dashed"),
    #plot.caption.position = "plot"
    plot.caption = element_text(hjust=0)
  )



title <- ggdraw() + 
  draw_label(
    "Measles Cases Hit Record \nHigh, 25 Years After U.S.\nEliminated the Disease",
    fontface = 'bold.italic',
    fontfamily = "Times New Roman",
    size = 40,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


plot_grid(title, p, 
          ncol = 1,
          # rel_heights values control vertical title margins
          rel_heights = c(0.45, 1)
          )



