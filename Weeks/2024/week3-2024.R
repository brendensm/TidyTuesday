library(tidytuesdayR)
library(tidyverse)
library(ggmap)

# loading data

tt <- tidytuesdayR::tt_load("2024", week = 3)

polling_places <- tt$polling_places

# exploration

# pp_2020 <- polling_places %>% filter(election_date == max(election_date))
#   
# 
# pp_st_count <- polling_places %>%
#   group_by(state, election_date) %>%
#   summarise(n = n())
# 
# pp_2020_st <- pp_2020 %>%
#   group_by(state) %>%
#   count()
# 
# st <- map_data("state")
# 
# pp_st_ct_20 <- pp_st_count %>% filter(election_date == max(election_date))
# 
# states <- usmap::statepop %>% select(-pop_2015)
# pp_st_ct_20m <- left_join(states, pp_st_ct_20, by = c("abbr" = "state"))
# 
# st %>% ggplot(aes(long, lat, group = group)) +
#   geom_polygon()
# 
# 
#  usmap::plot_usmap(data = pp_st_count, values = "n") +
#    scale_fill_continuous(low = "white", high = "blue") +
#    theme(legend.position = "right") +
#    facet_wrap(~election_date)
#    
#  
# pp_st_c_sub <- pp_st_count %>% filter(!c(election_date %in% c("2016-02-09", "2016-09-13")))
#  
# 
# usmap::plot_usmap(data = pp_st_c_sub, values = "n") +
#   scale_fill_continuous(low = "white", high = "blue") +
#   theme(legend.position = "right") +
#   facet_wrap(~election_date)
#  
# 
# pp_2020_st_merge <- left_join(pp_2020_ct, ct, by = c())

# polling_places %>% 
#   mutate(election_date_cat = as.character(election_date)) %>%
#   filter(!c(election_date_cat %in% c("2016-02-09", "2016-09-13"))) %>%
#   ggplot(aes(election_date_cat, fill = location_type)) +
#   geom_bar() #+
#   facet_wrap(~state)
#   
  
  
  
  

state_counts <- polling_places %>% 
  group_by(election_date, state) %>% 
  summarise(sites = n()) %>% 
  ungroup() %>%
  filter(!c(election_date %in% c("2016-02-09", "2016-09-13"))) %>%
  mutate(election_year = as.character(year(election_date)))


state_counts_wide <- state_counts %>%
  select(state, sites, election_year) %>%
  pivot_wider(values_from = sites, names_from = election_year) %>%
  select(state, `2012`, `2016`, `2020`) %>%
  drop_na()

complete_state_counts <- state_counts_wide %>%
  pivot_longer(!state, names_to = "election_year",
               values_to = "num_sites")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

facet_st_ct <- complete_state_counts %>%
  arrange(state, election_year) %>%
  mutate(change_sites = num_sites[match("2020", election_year)] - num_sites[match("2012", election_year)], 
              .by = state,
         ey_short = paste0("'", substrRight(election_year, 2)))

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# final plot

facet_st_ct %>%
  filter(election_year %in% c("2012", "2016", "2020")) %>%
  ggplot(aes(ey_short, num_sites, group = state)) +
  geom_line(aes(color = (change_sites < 0))) +
  geom_point(aes(color = (change_sites < 0)))+
  geom_text(y = 13500, aes(label = ifelse(ey_short=="'20", change_sites,""),
                           color = (change_sites < 0))) +
  ylim(0, 15000) +
  facet_wrap(~reorder(state, change_sites), ncol = 6) +
  theme_minimal(base_size = 14, base_family = "Courier") +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(x = "",
       y = "",
       title = "Changes in State Polling Locations 2012-2020",
       subtitle = "Among States with data for general elections in 2012, 2016, and 2020, \nmost have seen slight decreases in the number of polling locations. \nIllinois, Indiana, and Maryland saw the largest drop in the number of \npolling locations.",
       caption = "Brenden Smith | TidyTuesday Week 3, 2024 | Data: The Center for Public Integrity") +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    plot.background = element_rect(fill = "#FFFAFB"),
     panel.grid = element_line(color = "grey89")
  )













