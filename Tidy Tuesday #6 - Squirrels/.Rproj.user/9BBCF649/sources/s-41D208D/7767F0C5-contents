library(tidyverse)
library(ggthemr)
library(extrafont)
library(lubridate)
library(gganimate)
library(ggmap)
library(ggimage)

ggthemr(palette = "copper", type = "outer")

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

table(squirrels$date)

squirrels <- nyc_squirrels %>%
  mutate(date = mdy(date)) %>%
  mutate(day = day(date)) %>%
  arrange(day)

squirrel_days <- squirrels %>%
  group_by(day) %>%
  summarise(num_squirrels = n())

squirrel_map <- ggmap(get_googlemap("Central Park", maptype = "satellite", zoom = 14)) +
  stat_density2d(data = squirrels, aes(long, lat, fill = ..level..), geom = "polygon", alpha = 0.3) +
  scale_fill_continuous(low = "green", high = "red") +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), legend.position = "none") +
  labs(title = "Central Park Squirrel Density Map: Day {frame}") +
  theme(title = element_text(family = "Andale Mono")) +
  transition_manual(day)

squirrel_map
  
anim_save("squirrel_map.gif", squirrel_map)
