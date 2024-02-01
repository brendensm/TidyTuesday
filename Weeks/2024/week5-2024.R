library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2024-01-30')

groundhogs <- tuesdata$groundhogs
predictions <- tuesdata$predictions

gh_predictions <- 
  left_join(predictions, groundhogs, by = "id")
  
  
predictions |> 
  left_join()
  count(year, shadow) |> 
  drop_na() |> 
  filter(year > 1960) |> 
  ggplot(aes(year, n, color = shadow)) +
  geom_point() +
  geom_line()
  
  
  gh_predictions |> 
    count(year, region) |> 
    ggplot(aes(year, n, group = region)) +
    geom_line(aes(color = region))
  
  country_counts_year <- gh_predictions |> 
    group_by(country, year, shadow) |> 
    count() |> 
    drop_na() |> 
    pivot_wider(names_from = shadow,
                values_from = n) |> 
    mutate(total = sum(`TRUE`, `FALSE`, na.rm = TRUE),
           drop_see_shadow = `TRUE`/total)

  # I think I'm interested in groundhogs that aren't really groundhogs
  
  bar <- gh_predictions |> 
    group_by(is_groundhog) |> 
    count(shadow) |> 
    drop_na() |> 
    mutate(pct = n/sum(n),
           groundhog_cat = case_when(
             is_groundhog == "TRUE" ~ "Actual Groundhog",
             is_groundhog == "FALSE" ~ "Not a Groundhog",
             TRUE ~ NA
           ),
           saw_shadow = case_when(
             shadow == TRUE ~ "Yes",
             TRUE ~ "No"
           )) |> 
    ungroup() |> 
    ggplot(aes(y = pct, x= groundhog_cat, fill = saw_shadow)) +
    geom_col(position = "dodge") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "top",
          legend.title = element_text(size = 14)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("tomato", "darkgreen")) +
    labs(y = "", x = "", fill = "Did it see it's shadow?") 
    
  
  library(ggwordcloud)
  
wc <- groundhogs |> 
    filter(is_groundhog == "FALSE") |> 
    mutate(type = case_when(type == "Taxidermied groundhogs" ~ "Taxidermied groundhog",
                            TRUE ~ type)) |> 
    count(type) |> 
    ungroup() |> 
    ggplot(aes(label = type, size = n, color = n)) +
    geom_text_wordcloud(shape = "square") +
    theme_minimal(base_size = 16) +
    scale_size_area(max_size = 10) +
   scale_color_gradient(low = "darkgreen", high = "#673E0F")
    
  row <- cowplot::plot_grid(bar, wc, ncol = 2) 
  
  
  title <- ggdraw() + 
    draw_label(
      "Real Groundhogs and Imposters",
      fontface = 'bold',
      x = 0,
      hjust = 0, size = 20
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(-20, 0, -25, 7)
    )
  
  
  
  subtitle <- ggdraw() +
    draw_label(
      "A higher proportion of actual groundhogs did not see their shadow compared to imposters. Non-groundhogs \ninclude taxidermied groundhogs, people dressing up as groundhogs, and a variety of other animals.",
      x = 0,
      hjust = 0,
      size = 14
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(-20, 0, -25, 8)
    )
  
  caption <- ggdraw() +
    draw_label(
      "Brenden Smith | TidyTuesday Week 5, 2024 | Data: groundhog-day.com",
      x = 0,
      hjust = 0,
      size = 12
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(-20, 0, 0, 290)
    )
  
  
  cowplot::plot_grid(
    title, subtitle, row, caption,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 0.1, 1, 0.05)
  )
  


    
  