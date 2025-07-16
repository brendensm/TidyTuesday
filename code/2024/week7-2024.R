library(tidyverse)
library(janitor)
library(ggimage)
library(ggsvg)
library(svgparser)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2024, 7)
hist_spend <- tuesdata$historical_spending |> clean_names()
gifts_age <- tuesdata$gifts_age |> clean_names()
gifts_gender <- tuesdata$gifts_gender |> clean_names()


svg_text <- '<svg version="1.0" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" 
  width="800px" height="800px" viewBox="0 0 64 64" enable-background="new 0 0 64 64" xml:space="preserve">
    <g>
    <path fill="{{innerfill}}" d="M58.714,29.977c0,0-0.612,0.75-1.823,1.961S33.414,55.414,33.414,55.414C33.023,55.805,32.512,56,32,56
		s-1.023-0.195-1.414-0.586c0,0-22.266-22.266-23.477-23.477s-1.823-1.961-1.823-1.961C3.245,27.545,2,24.424,2,21
		C2,13.268,8.268,7,16,7c3.866,0,7.366,1.566,9.899,4.101l0.009-0.009l4.678,4.677c0.781,0.781,2.047,0.781,2.828,0l4.678-4.677
		l0.009,0.009C40.634,8.566,44.134,7,48,7c7.732,0,14,6.268,14,14C62,24.424,60.755,27.545,58.714,29.977z"/>
    <path fill="{{innerfill}}" d="M58.714,29.977c0,0-0.612,0.75-1.823,1.961S33.414,55.414,33.414,55.414C33.023,55.805,32.512,56,32,56
		s-1.023-0.195-1.414-0.586c0,0-22.266-22.266-23.477-23.477s-1.823-1.961-1.823-1.961C3.245,27.545,2,24.424,2,21
		C2,13.268,8.268,7,16,7c3.866,0,7.366,1.566,9.899,4.101l0.009-0.009l4.678,4.677c0.781,0.781,2.047,0.781,2.828,0l4.678-4.677
		l0.009,0.009C40.634,8.566,44.134,7,48,7c7.732,0,14,6.268,14,14C62,24.424,60.755,27.545,58.714,29.977z"/>
    <g>
    <path fill="#000000" d="M48,5c-4.418,0-8.418,1.791-11.313,4.687l-3.979,3.961c-0.391,0.391-1.023,0.391-1.414,0
			c0,0-3.971-3.97-3.979-3.961C24.418,6.791,20.418,5,16,5C7.163,5,0,12.163,0,21c0,3.338,1.024,6.436,2.773,9
			c0,0,0.734,1.164,1.602,2.031s24.797,24.797,24.797,24.797C29.953,57.609,30.977,58,32,58s2.047-0.391,2.828-1.172
			c0,0,23.93-23.93,24.797-24.797S61.227,30,61.227,30C62.976,27.436,64,24.338,64,21C64,12.163,56.837,5,48,5z M58.714,29.977
			c0,0-0.612,0.75-1.823,1.961S33.414,55.414,33.414,55.414C33.023,55.805,32.512,56,32,56s-1.023-0.195-1.414-0.586
			c0,0-22.266-22.266-23.477-23.477s-1.823-1.961-1.823-1.961C3.245,27.545,2,24.424,2,21C2,13.268,8.268,7,16,7
			c3.866,0,7.366,1.566,9.899,4.101l0.009-0.009l4.678,4.677c0.781,0.781,2.047,0.781,2.828,0l4.678-4.677l0.009,0.009
			C40.634,8.566,44.134,7,48,7c7.732,0,14,6.268,14,14C62,24.424,60.755,27.545,58.714,29.977z"/>
    <path fill="#000000" d="M48,11c-0.553,0-1,0.447-1,1s0.447,1,1,1c4.418,0,8,3.582,8,8c0,0.553,0.447,1,1,1s1-0.447,1-1
			C58,15.478,53.522,11,48,11z"/>
    </g>
    </g>
    </svg>'

colors <- c('#EEA7CB', '#BD5353')

names(colors) <- c("Per Person Spending", "Percent of People Celebrating")

title_text <- glue::glue(
  'Trends of ',
  '<span style = "color:{colors["Per Person Spending"]}">**per person spending**</span> ',
  "and ",
  '<span style = "color:{colors["Percent of People Celebrating"]}">**percent of people celebrating**</span> ', 
  '<br></br>Valentine\'s day, 2010-2022'
)

hist_spend |> 
  pivot_longer(!year) |> 
  mutate(year = ymd(year, truncated = 2L)) |> 
  filter(name == "per_person" | name == "percent_celebrating") |> 
  ggplot(aes(x = year, y = value, group = name, color = name)) +
  geom_line(linewidth = 1) +
    geom_point_svg(aes(x = year, y = value, innerfill = name), 
                   svg = svg_text, defaults = list(
                                                   innerfill = "#aaa")) +
  scale_svg_fill_manual(
    aesthetics = 'innerfill',
    values = c(per_person = '#EEA7CB', percent_celebrating = '#BD5353')
    
  ) +
  labs(title = title_text, x = "", y = "", caption = "Brenden Smith | TidyTuesday Week 7, 2024") +
  scale_color_manual(values = c(per_person = '#EEA7CB', percent_celebrating = '#BD5353')) +
  theme_minimal(base_size = 20, base_family = "Noteworthy", base_line_size = .25) +
  theme(legend.position = "none",
        plot.background = element_rect( "#43387F", color = "#43387F"),
        panel.grid = element_line("#F9E0F0"),
       # panel.grid.minor = element_blank(),
       # panel.grid.major = element_blank(),
        axis.text = element_text(color = "#F9E0F0"),
        axis.title = element_text(color = "#F9E0F0"),
        plot.title = ggtext::element_markdown(color = "#F9E0F0", size = 26),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "#F9E0F0"),
        plot.caption = element_text(color = "#F9E0F0")) 
  



  