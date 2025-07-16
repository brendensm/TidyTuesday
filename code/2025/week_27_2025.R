# TidyTuesday!!!

library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(showtext)

source("code-for-posting/socials.R")

sysfonts::font_add("Sans", "/System/Library/Fonts/Supplemental/Sans.ttc")
 showtext_auto()
 caption <- social_caption("brendensm", "beesm", "brendenmsmith", icon_color =  "#703be7", text_color = "#340042")



tuesdata <- tt_load(2025, 27)

list2env(tuesdata, envir = .GlobalEnv)

readme(tuesdata)

lapply(tuesdata, names)

users |> count(colorblind)

# 1 "#8240EA"    1 purple "#7e1e9c"
# "#4B31EA"     3 blue   "#0343df"


caption


count_of_answers <- answers |> 
  count(user_id, sort=T)


users |> 
  left_join(count_of_answers, by = "user_id") |> 
  rename(correct_answers = n) |> 
  group_by(colorblind) |> 
  summarise(sum_cor = sum(correct_answers, na.rm = TRUE),
            num_in_group = n()) |> 
  mutate(rate_cor = sum_cor/num_in_group)



users |> 
  left_join(count_of_answers, by = "user_id") |> 
  rename(cc_answers = n) |> 
  mutate(cc_answers = case_when(
    is.na(cc_answers) ~ 0,
    TRUE~ cc_answers
  )) |> 
  group_by(y_chromosome, colorblind) |> 
  summarise(sum_cc = sum(cc_answers, na.rm = TRUE),
            num_in_group = n()) |> 
  mutate(rate_cor = sum_cc/num_in_group) |> 
  drop_na() |> 
  mutate(group = case_when(
    y_chromosome ==1 & colorblind==1 ~ "Y, CB",
    y_chromosome ==1 & colorblind==0 ~ "Y, NCB",
    y_chromosome ==0 & colorblind==1 ~ "NY, CB",
    y_chromosome ==0 & colorblind==0 ~ "NY, NCB"
  )) |> 
  ggplot(aes(sum_cc, group, fill = group)) +
  geom_col()


ten_colors <- answers


ten_colors$hex |> length()

color_df <- data.frame(
  hex = ten_colors$hex,
  x = rep(1:ceiling(sqrt(length(ten_colors$hex))), length.out = length(ten_colors$hex)),
  y = rep(1:ceiling(length(ten_colors$hex)/ceiling(sqrt(length(ten_colors$hex)))), each = ceiling(sqrt(length(ten_colors$hex))))[1:length(ten_colors$hex)]#,
  #name = ten_colors$color
  )


color_df |> 
  ggplot(aes(x = x, y = y)) +
  geom_tile(fill = color_df$hex, color = "white", size = 1) +
#  geom_text(aes(label = name), color = "black", size = 5) +
  coord_equal() +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


"#533cc6"
unique(answers$user_id) %in% unique(users$user_id) |> sum()


library(tidytext)

color_ranks |> 
  tidytext::unnest_tokens(word, color) |> 
  select(word) |> 
  drop_na() |> 
 # anti_join(stop_words) |> 
  count(word, sort = T) |> View()
#  filter(!str_detect(word, "red|green|yellow|blue|purple|pink|brown|grey|orange|lime|teal|olive|violet|aqua|rose|tan|lavender|turquoise|magenta|navy")) |> 
  top_n(10) |> 
  ggplot(aes(n, reorder(word, -n))) +
  geom_col(fill = "#703be7") +
  #geom_text(aes(label = word), color = "white", size = 5, hjust = 2) +
  geom_text(aes(label = n), size = 5.5, nudge_x = -2.5, color = "white") +
  labs(title = "Top ten color adjectives among the 949 most common named colors") +
  theme_minimal(base_size = 16) +
  theme(
        plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18)
        )
#  theme_void()




color_ranks |> 
  tidytext::unnest_tokens(word, color) |> 
  select(word) |> 
  drop_na() |> 
  # anti_join(stop_words) |> 
  count(word, sort = T) |> 
  filter(!str_detect(word, "red|green|yellow|blue|purple|pink|brown|grey|orange|lime|teal|olive|violet|aqua|rose|tan|lavender|turquoise|magenta|navy")) |> 
  top_n(10) |> 
  ggplot(aes(n, reorder(word, -n))) +
  geom_col(fill = "#703be7") +
  geom_text(aes(label = word), x = 0.5, color = "#ece5fc", size = 5.5, hjust = 0) +
  geom_text(aes(label = n), size = 5.5, nudge_x = 1.5, color = "#340042") +
  labs(title = "The top ten xkcd color survey adjectives",
       subtitle = "Among the 949 most commonly identified rgb colors",
       caption = paste("TidyTuesday Week 27, 2025 |", caption)) +
  theme_void(base_size = 16) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05)), limits = c(0, NA)) +
  theme(
    plot.background = element_rect(fill = "#ece5fc"),
    plot.subtitle = element_text(margin = margin(t=5,b=0, l=5), color = "#340042"),
    plot.caption = element_textbox_simple(margin = margin(t=5,b=5, l=5)),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", color = "#340042", margin = margin(l=5, t=5 ))
    )






library(ggplot2)
library(dplyr)

# Assuming your data looks something like this:
# df <- data.frame(hex_code = your_hex_codes, color_group = your_groups)


dist <- answers |> distinct(hex, rank) |> 
  left_join(color_ranks, by = "rank") |> 
  rename(hex_code = hex.x) |> 
  select(-hex.y)

df <- dist[1:50000,]

# Calculate grid dimensions for a roughly square layout
n_colors <- nrow(df)
n_cols <- ceiling(sqrt(n_colors))
n_rows <- ceiling(n_colors / n_cols)

# Make sure your coordinates are tight with no gaps
df <- df %>%
  group_by(color) %>%
  arrange(color) %>%
  mutate(
    # Create a sequential index within each group
    index = row_number(),
    # Calculate grid dimensions for this group
    n_colors = n(),
    n_cols = ceiling(sqrt(n_colors)),
    # Assign coordinates with no gaps
    x = ((index - 1) %% n_cols) + 1,
    y = ceiling(index / n_cols)
  ) %>%
  ungroup()

# Create the plot
ggplot(df, aes(x = x, y = y, fill = hex_code)) +
  geom_tile() +
  scale_fill_identity() +  # This uses the hex codes directly as colors
  facet_wrap(~color) +  # Separate panel for each color group
  theme_void() +  # Remove axes, gridlines, etc.
  theme(
    strip.text = element_text(size = 12),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_equal()  # Keep squares actually square


ggplot(df, aes(x = x, y = y, fill = hex_code)) +
  geom_raster() +  # No gaps by default
  scale_fill_identity() +
  facet_wrap(~color, ncol = 5) +
  theme_void() +
  coord_equal()



# Set a fixed width (number of columns) for all groups
fixed_cols <- 50  # Adjust this number to your preference

df <- df %>%
  group_by(color) %>%
  arrange(color) %>%
  mutate(
    index = row_number(),
    x = ((index - 1) %% fixed_cols) + 1,
    y = ceiling(index / fixed_cols)
  ) %>%
  ungroup()
df |> 
  mutate(color = factor(color, levels = c("green", "blue", "purple", "pink", "brown"))) |> 
ggplot( aes(x = x, y = y, fill = hex_code)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_identity() +
 # scale_x_continuous(expand = c(0, 0), limits = c(0.5, fixed_cols + 0.5)) +
 # scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~color, nrow=1) +
  theme_void(base_size = 16) +
  theme(
    panel.spacing = unit(0.1, "lines"),
    plot.margin = margin(5, 5, 5, 5),
    strip.text = element_text(margin = margin(b=5))
  ) +
  coord_equal()











# Set a fixed width (number of columns) for all groups
fixed_cols <- 40  # Adjust this number to your preference

df2 <- df %>%
  group_by(color) %>%
  arrange(color) %>%
  mutate(
    index = row_number(),
    y = ((index - 1) %% fixed_cols) + 1,
    x = ceiling(index / fixed_cols),
    color_group_styled = factor(case_when(
      color == "blue" ~ "<span style='color:blue'>blue</span>",
      color == "green" ~ "<span style='color:green4'>green</span>",
      color == "brown" ~ "<span style='color:brown'>brown</span>",
      color == "pink" ~ "<span style='color:deeppink'>pink</span>",
      color == "purple" ~ "<span style='color:purple'>purple</span>",
      TRUE ~ color
    ),
    levels = c(
 "<span style='color:green4'>green</span>",
"<span style='color:blue'>blue</span>",
"<span style='color:purple'>purple</span>",
"<span style='color:deeppink'>pink</span>",
 "<span style='color:brown'>brown</span>"
    ))
  ) %>%
  ungroup()



library(grDevices)

# Function to convert hex to HSV and extract brightness
get_brightness <- function(hex_colors) {
  # Convert hex to RGB
  rgb_vals <- col2rgb(hex_colors)
  # Convert RGB to HSV and extract the V (brightness) component
  hsv_vals <- rgb2hsv(rgb_vals)
  return(hsv_vals[3, ])  # Return the V (brightness/value) component
}

get_luminance <- function(hex_colors) {
  rgb_vals <- col2rgb(hex_colors)
  # Convert to 0-1 scale
  r <- rgb_vals[1, ] / 255
  g <- rgb_vals[2, ] / 255
  b <- rgb_vals[3, ] / 255
  
  # Calculate perceived luminance using standard formula
  luminance <- 0.299 * r + 0.587 * g + 0.114 * b
  return(luminance)
}

get_hue <- function(hex_colors) {
  rgb_vals <- col2rgb(hex_colors)
  hsv_vals <- rgb2hsv(rgb_vals)
  return(hsv_vals[1, ])  # Return the H (hue) component
}

# Sort within each color group by brightness
df2 <- df %>%
  group_by(color) %>%
  mutate(luminance = get_hue(hex_code)) %>%
  arrange(color, desc(luminance)) %>%  # desc() for light to dark
  mutate(
    index = row_number(),
    y = ((index - 1) %% fixed_cols) + 1,
    x = ceiling(index / fixed_cols),
    color_group_styled = factor(case_when(
      color == "blue" ~ "<span style='color:blue'>blue</span>",
      color == "green" ~ "<span style='color:green4'>green</span>",
      color == "brown" ~ "<span style='color:brown'>brown</span>",
      color == "pink" ~ "<span style='color:deeppink'>pink</span>",
      color == "purple" ~ "<span style='color:purple'>purple</span>",
      TRUE ~ color
    ),
    levels = c(
      "<span style='color:green4'>green</span>",
      "<span style='color:blue'>blue</span>",
      "<span style='color:purple'>purple</span>",
      "<span style='color:deeppink'>pink</span>",
      "<span style='color:brown'>brown</span>"
    ))
  ) %>%
  ungroup()



get_hue_sat_val <- function(hex_colors) {
  rgb_vals <- col2rgb(hex_colors)
  hsv_vals <- rgb2hsv(rgb_vals)
  return(data.frame(
    hue = hsv_vals[1, ],
    saturation = hsv_vals[2, ],
    value = hsv_vals[3, ]
  ))
}

df2 <- df %>%
  group_by(color) %>%
  bind_cols(get_hue_sat_val(.$hex_code)) %>%
  arrange(color, hue, desc(saturation), desc(value)) %>%  # Sort by hue first, then sat, then brightness
  mutate(
    index = row_number(),
    y = ((index - 1) %% fixed_cols) + 1,
    x = ceiling(index / fixed_cols),
    color_group_styled = factor(case_when(
      color == "blue" ~ "<span style='color:blue'>blue</span>",
      color == "green" ~ "<span style='color:green4'>green</span>",
      color == "brown" ~ "<span style='color:brown'>brown</span>",
      color == "pink" ~ "<span style='color:deeppink'>pink</span>",
      color == "purple" ~ "<span style='color:purple'>purple</span>",
      TRUE ~ color
    ),
    levels = c(
      "<span style='color:green4'>green</span>",
      "<span style='color:blue'>blue</span>",
      "<span style='color:purple'>purple</span>",
      "<span style='color:deeppink'>pink</span>",
      "<span style='color:brown'>brown</span>"
    ))
  ) %>%
  ungroup()



get_lab <- function(hex_colors) {
  lab_vals <- as(hex2RGB(hex_colors), "LAB")
  return(data.frame(
    L = lab_vals@coords[, "L"],  # Lightness
    A = lab_vals@coords[, "A"],  # Green-Red
    B = lab_vals@coords[, "B"]   # Blue-Yellow
  ))
}

library(colorspace)


df2 <- df %>%
  group_by(color) %>%
  bind_cols(get_lab(.$hex_code)) %>%
  arrange(color, A, B, desc(L)) %>%  # Sort by color dimensions, then lightness
  mutate(
    index = row_number(),
    y = ((index - 1) %% fixed_cols) + 1,
    x = ceiling(index / fixed_cols),
    color_group_styled = factor(case_when(
      color == "blue" ~ "<span style='color:blue'>blue</span>",
      color == "green" ~ "<span style='color:green4'>green</span>",
      color == "brown" ~ "<span style='color:brown'>brown</span>",
      color == "pink" ~ "<span style='color:deeppink'>pink</span>",
      color == "purple" ~ "<span style='color:purple'>purple</span>",
      TRUE ~ color
    ),
    levels = c(
      "<span style='color:green4'>green</span>",
      "<span style='color:blue'>blue</span>",
      "<span style='color:purple'>purple</span>",
      "<span style='color:deeppink'>pink</span>",
      "<span style='color:brown'>brown</span>"
    ))
  ) %>%
  ungroup()



# Define reference colors for each group (pure versions)
reference_colors <- c(
  "blue" = "#0000FF",
  "green" = "#00FF00", 
  "brown" = "#8B4513",
  "pink" = "#FF69B4",
  "purple" = "#800080"
)

# Function to calculate color distance
color_distance <- function(hex1, hex2) {
  rgb1 <- col2rgb(hex1)
  rgb2 <- col2rgb(hex2)
  sqrt(sum((rgb1 - rgb2)^2))
}

reference_colors <- c(
  "blue" = "#0000FF",
  "green" = "#00FF00", 
  "brown" = "#8B4513",
  "pink" = "#FF69B4",
  "purple" = "#800080"
)

df2 <- df %>%
  group_by(color) %>%
  mutate(
    ref_color = reference_colors[color],
    distance = map2_dbl(hex_code, ref_color, color_distance)
  ) %>%
  arrange(color, distance) %>%  # Sort by distance from reference
  mutate(
    index = row_number(),
    y = ((index - 1) %% fixed_cols) + 1,
    x = ceiling(index / fixed_cols),
    color_group_styled = factor(case_when(
      color == "blue" ~ "<span style='color:blue'>blue</span>",
      color == "green" ~ "<span style='color:green4'>green</span>",
      color == "brown" ~ "<span style='color:brown'>brown</span>",
      color == "pink" ~ "<span style='color:deeppink'>pink</span>",
      color == "purple" ~ "<span style='color:purple'>purple</span>",
      TRUE ~ color
    ),
    levels = c(
      "<span style='color:green4'>green</span>",
      "<span style='color:blue'>blue</span>",
      "<span style='color:purple'>purple</span>",
      "<span style='color:deeppink'>pink</span>",
      "<span style='color:brown'>brown</span>"
    ))
  ) %>%
  ungroup()

df2 |> 
#  mutate(color = factor(color, levels = c("green", "blue", "purple", "pink", "brown"))) |> 
  ggplot( aes(x = x, y = y, fill = hex_code)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_identity() +
  facet_wrap(~color_group_styled, ncol=1) +
  theme_void(base_size = 20) +
 # labs(title = "Over one million colors") +
  theme(
    panel.spacing = unit(0.1, "lines"),
    plot.margin = margin(rep(0, 5)),
    strip.text = element_markdown(
      hjust = 0.05,
      face = "bold",
      margin = margin(b=5))
  ) +
  coord_equal()












library(tidytext)
library(dplyr)
library(stringr)

# First, tokenize the color names into words
color_words <- color_ranks %>%
  unnest_tokens(word, color, token = "words") %>%
  group_by(rank, hex) %>%
  mutate(word_position = row_number()) %>%
  ungroup()

# Identify color base words vs modifiers
#"red|green|yellow|blue|purple|pink|brown|grey|orange|lime|teal|olive|violet|aqua|rose|tan|lavender|turquoise|magenta|navy")) |> 
  
# You might need to create a list of base color words
base_colors <- c("purple", "green", "blue", "pink", "brown", "red", "lime",
                 "orange", "yellow", "white", "black", "grey", "gray", "aqua",
                 "teal", "cyan", "magenta", "violet", "indigo", "rose", "olive", "lavender", "tan", "turquoise", "navy")

# Find adjectives/modifiers (words that aren't base colors)
modifiers <- color_words %>%
  filter(!word %in% base_colors) %>%
  count(word, sort = TRUE)

# Look at modifier frequency
modifiers #|> 

# For co-occurrence analysis, you can also do:
color_bigrams <- color_ranks %>%
  unnest_tokens(bigram, color, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!is.na(word2))

# Find which words commonly precede base colors
preceding_words <- color_bigrams %>%
  filter(word2 %in% base_colors) %>%
  count(word1, word2, sort = TRUE)

# Find which words commonly follow base colors  
following_words <- color_bigrams %>%
  filter(word1 %in% base_colors) %>%
  count(word1, word2, sort = TRUE)

top_adjectives <- color_words %>%
  filter(!word %in% base_colors) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 10) %>%  # Top 10 adjectives
  pull(word)

 color_ranks %>%
  unnest_tokens(bigram, color, token = "ngrams", n= 2) |> View()# %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!is.na(word2)) %>%
  filter(word1 %in% top_adjectives | word2 %in% top_adjectives) %>%
  mutate(
    adjective = ifelse(word1 %in% top_adjectives, word1, word2),
    base_color = ifelse(word1 %in% base_colors, word1, word2)
  ) %>%
  filter(!is.na(base_color)) %>%
  count(adjective, base_color, sort = TRUE)

# Visualization 1: Heatmap of adjective-color combinations
ggplot(adjective_color_combos, aes(x = base_color, y = adjective, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Most Common Adjective-Color Combinations",
       x = "Base Color", y = "Adjective", fill = "Count")

# Visualization 2: Bar chart showing top combinations
top_combos <- adjective_color_combos %>%
  slice_head(n = 15) %>%
  mutate(combo = paste(adjective, base_color))

ggplot(top_combos, aes(x = reorder(combo, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Adjective-Color Combinations",
       x = "Combination", y = "Count")

# Visualization 3: Network-style plot (if you want to get fancy)
library(ggraph)
library(igraph)

# Create network data
network_data <- adjective_color_combos %>%
  filter(adjective %in% "baby"|base_color%in%"baby") %>%  # Only show combinations that appear at least twice
  select(from = adjective, to = base_color, weight = n)

graph <- graph_from_data_frame(network_data, directed = FALSE)

ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.6) +
  geom_node_point(size = 4, color = "steelblue") +
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 0.5) +
  theme_void() +
  labs(title = "Adjective-Color Network") +
  scale_edge_width(range = c(0.5, 2))





color_ranks |> 
  separate_wider_delim(color, delim = " ", 
                       names_sep = "_", 
                       too_few = "align_start") |> 
  count(color_1, sort=T)
  View()

