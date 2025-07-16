library(tidyverse)
library(tidytuesdayR)
library(wordcloud2)
library(tidytext)
library(cowplot)

d <- tt_load(2025, 5)


script_lines <- d$simpsons_script_lines 

hf <- script_lines |> 
  filter(character_id == 2) |> 
  unnest_tokens(word, spoken_words) |> 
  select(word) |> 
  drop_na() |> 
  anti_join(stop_words) |> 
  count(word)

sub <- hf |> mutate(n = sqrt(n))

wordcloud2(filter(hf, n>1),
           figPath = "home.jpg",
           size = .65)

mf <- script_lines |> 
  filter(character_id == 1) |> 
  unnest_tokens(word, spoken_words) |> 
  select(word) |> 
  drop_na() |> 
  anti_join(stop_words) |> 
  count(word)


wordcloud2(filter(mf, n>0),
           figPath = "marge2.jpg",
           size = .65)


# Create the shaped word cloud
wordcloud2(demoFreq,size = 2,figPath = "home.jpg") 






sysfonts::font_add("Futura", "/System/Library/Fonts/Supplemental/Futura.ttc")
showtext::showtext_auto()

caption <- social_caption("brendensm", "beesm", "brendenmsmith",
                          icon_color = "#1D0C2A", text_color = "black",
                          family = "Futura")





h2 <- ggdraw() + draw_image("screenshots/h.jpg",
                            scale = .6 )
m2 <- ggdraw() + draw_image("screenshots/m.jpg",
                            scale = .8) +
  theme(plot.margin = margin(b = 65))

clouds <- plot_grid(h2, m2, labels = c("Homer", "Marge"))

title <- ggdraw() + 
  draw_label(
    "The Simpson's dialogue (2010-2016)",
  #  fontfamily = "Futura",
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


plot_grid(title, clouds, rel_heights = c(.1, .9),
          nrow = 2)  
  


