library(tidyverse)
library(ggtext)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load(2023, week = 42)

taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

theme_set(theme_minimal())


dance <- taylor_album_songs %>%
  group_by(album_name) %>%
  summarise(mean_dance = mean(danceability, na.rm = T))

key <- taylor_album_songs %>%
  group_by(key_name) %>%
  count()

taylor_album_songs %>%
  ggplot(aes(duration_ms, track_release, color = album_name)) +
  geom_point()

taylor_album_songs %>%
  group_by(album_name, key_name) %>%
  summarise(key_n = n())  %>%
  mutate(songs_n = sum(key_n), pct = key_n/songs_n)%>%
  ggplot(aes(x = album_name, y = pct, fill = key_name)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  labs(x = "")

taylor_albums %>%
  mutate(metacritic_score = metacritic_score/10) %>%
  select(-ep) %>%
  pivot_longer(-c(album_name, album_release), names_to = "review", values_to = "score") %>%
  drop_na() %>%
  ggplot(aes(score, reorder(album_name, album_release), fill = album_name)) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(1, 10)) +
  facet_wrap(~review, ncol = 1) 
  
taylor_albums %>%
  drop_na() %>%
  ggplot(aes(user_score, reorder(album_name, album_release), fill = album_name)) +
  geom_bar(stat = "identity")

# Read in album artwork

mypath = "images/tswift/" #file where the images are
files <- list.files(path=mypath, pattern=".jpg$")
length(files)

for (i in files){
  im <- magick::image_read(paste0(mypath, i))
  im.convert <- as.vector(im)
  out <- as.data.frame(do.call(rbind, im.convert)) # create a data frame
  return(out)
}



swiz <- png::readPNG("images/tswift/swizzle.png")

# Arranging images thanks to Nicola Rennie! https://github.com/nrennie/tidytuesday/tree/main/2023/2023-10-17
imgdata <- taylor_albums %>%
  drop_na() %>%
  select(album_name) %>%
  mutate(
    img = str_replace(album_name, " \\s*\\([^\\)]+\\)", ""),
    img = str_to_lower(str_replace(img, " ", "")),
    img = case_when(
      album_name == "Red (Taylor's Version)" ~ "redtaylorsversion",
      album_name == "Fearless (Taylor's Version)" ~ "fearlesstaylorsversion",
      TRUE ~ img
    ),
    img = paste0(img, ".jpg"),
    img = file.path("images", "tswift", img)
  )






colors <- c("#D8588D", "#45A9CD")

names(colors) <- c("Metacritic", "User")

title_text <- glue::glue(
  "Differences between ",
  '<span style = "color:{colors["Metacritic"]}">**metacritic**</span> ',
  "and ",
  '<span style = "color:{colors["User"]}">**user**</span> ', 
  "scores"
)

taylor_albums %>%
  mutate(metacritic_score = metacritic_score/10) %>%
  select(-ep) %>%
  pivot_longer(-c(album_name, album_release), names_to = "review", values_to = "score") %>%
  drop_na() %>%
  mutate(review = case_when(review == "metacritic_score" ~ "Metacritic",
                            TRUE ~ "User")) %>%
  ggplot(aes(score, reorder(album_name, album_release))) +
  geom_line(
    color = "#333333",
    linewidth = 1
  ) +
  geom_point(
    aes(fill = review),
    shape = 21,
    size = 5,
    color = "#333333",
    stroke = 1
  ) +
  scale_fill_manual(values = colors) +
   scale_y_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                   width = 18)) +
  theme_minimal(base_size = 14, base_family = "PT Serif") +
  labs(title = "Taylor Swift Album Ratings",
       x = "", y = "",
       subtitle = title_text,
       caption = "Brenden Smith | Data: taylor R package | Metacritic score is divided by 10 to comapre with user scores.") +
  theme(legend.position = "none",
        plot.title.position = "plot",
      plot.title = element_text(size = 26),
        plot.subtitle = ggtext::element_markdown(size = 18),
      plot.background = element_rect(fill = "#FFFFEB"),
      plot.caption = element_text(size = 12, family = "PT Serif Caption"),
      panel.grid.major = element_line(color = "grey80"),
      axis.text = element_text(color = "grey14")) +
    annotation_custom(grob = grid::rasterGrob(swiz, interpolate = TRUE, 
                                          width=unit(3.75,'cm'),
                                          x = unit(1,"npc"), y = unit(1,"npc"),
                                          hjust = 1, vjust=2.2)) +
  ggimage::geom_image(
    data = imgdata,
    mapping = aes(y = album_name, x = 6.25, image = img),
    asp = 1,
    size = 0.08
  )


