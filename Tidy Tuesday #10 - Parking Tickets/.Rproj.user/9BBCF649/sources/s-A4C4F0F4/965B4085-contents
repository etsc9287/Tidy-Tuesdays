library(tidyverse)
library(lubridate)
library(ggmap)
library(gganimate)
library(ggthemr)
library(extrafont)


ggthemr(palette = "earth", type = "outer")

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

tickets2 <- tickets %>%
  mutate(hour = hour(issue_datetime)) %>%
  mutate(month = month(issue_datetime, label = TRUE)) %>%
  sample_n(100000)

ticket_map <- ggmap(get_googlemap("Philadelphia", maptype = "roadmap", zoom = 14)) +
  stat_density2d(data = tickets2, aes(lon, lat, fill = ..level..), geom = "polygon", alpha = 0.3) +
  scale_fill_continuous(low = "green", high = "red") +
  ggtitle("Parking Tickets in Downtown Philadelphia", subtitle = "Hour {frame}") +
  labs(fill = "Tickets") +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  transition_manual(hour)

anim_save("ticket_map.gif", ticket_map)  
