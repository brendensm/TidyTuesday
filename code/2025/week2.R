library(tidyverse)
library(rsvg)
library(ggimage)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2025-01-14')

conf2023 <- tuesdata$conf2023
conf2024 <- tuesdata$conf2024
  
title_words <-   select(conf2023, session_title) |> mutate(year = "2023") |> 
    rbind(mutate(select(rename(conf2024, session_title = talk_title), session_title), year = "2024")) |> 
  distinct() |> 
    unnest_tokens(word, session_title)

title_tokens <- title_words |> 
  anti_join(stop_words)
  
top20 <- title_tokens |> 
  count(year, word) |>
  top_n(20) 
  
  d <- title_tokens %>%
    count(year, word) %>%
    pivot_wider(names_from = year, values_from = n) %>%
    filter(word %in% unique(top20$word)) %>%
    mutate(dif = `2024` - `2023` ,
           color = case_when(                        
             dif < 0 ~ "neg",
             dif > 0 ~ "pos",
             TRUE ~ NA_character_
           ),
           rel_change = dif/`2023`) 
  
    svg_grob <- rasterGrob(rsvg::rsvg("images/posit-logo-2024.svg"), 
                           interpolate = TRUE)
    
    d |> 
      ggplot(aes(x = 0, xend = rel_change, y = reorder(word, rel_change))) +
      geom_segment( 
        size = 1.1,
        arrow = arrow(type = "closed", length = unit(0.25, "cm")), 
        aes(color = color)
      ) +
      geom_text(data = filter(d, rel_change != 0), 
                aes(x = rel_change, hjust = ifelse(dif > 0, -0.3, 1.3), 
                    color = color,
                    label = scales::number(dif, style_positive = "plus", 
                                           style_negative = "hyphen")), 
                size = 5, 
                 vjust = 0.5) +  
      geom_text(data = filter(d, rel_change == 0), aes(label = "l"), 
                size = 7, color = "#4D4D4D") +
      annotation_custom(svg_grob, xmin = 0, xmax = 7.4, ymin = .5, ymax = 2.5) +
      theme_minimal(base_size = 24, base_family = "Optima") +
      theme(legend.position = "none",
            axis.title = element_blank(),
            plot.title.position = "plot") +
      scale_color_manual(values = c( "#EE6331", "#447098"), na.value = "transparent") +
      scale_x_continuous(labels = scales::percent) +
      labs(
        caption = "Graphic: Brenden Smith | TidyTuesday Week 2, 2025",
        title = str_wrap("Relative change in the twenty most common words in talk titles between posit::conf 2023 and 2024", 65)
      )
