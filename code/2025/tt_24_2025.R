library(tidyverse)
library(tidytuesdayR)

tuesdata <- tt_load(2025, 24)

list2env(tuesdata, envir = .GlobalEnv)

tidytuesdayR::readme(tuesdata)

api_categories |> count(apisguru_category, sort=T)

api_info
api_origins |> count(format)


api_categories |> count(name, sort=T)

# Top 20 categories as a circular plot?

cat_20 <- api_categories |> count(apisguru_category, sort=F) |> 
  filter(n >= 16) |> 
  rename(cat = apisguru_category) |> 
  mutate(id = seq(1, 20))

cat_20

number_of_bar <- nrow(cat_20)

label_data <- cat_20

angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
p <- ggplot(cat_20, aes(x=as.factor(id), y=n)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-300,1180) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=n+10, label=cat, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p








# top API providers?

prov10 <- api_info |> count(provider_name,
                  sort = T) |> 
  filter(n >= 23)

name_url <- api_logos |> filter(str_detect(name, paste(prov10$provider_name, collapse = "|"))) |> 
  mutate(name = str_replace(name, ":.*", "")) |> 
  count(name, url) |> filter(name %in% prov10$provider_name &
                               !str_detect(url, "favicon|YouTube")) |> 
  select(-n)





