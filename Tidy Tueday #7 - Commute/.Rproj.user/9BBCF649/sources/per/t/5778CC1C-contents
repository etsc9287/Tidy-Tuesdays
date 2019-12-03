library(tidyverse)
library(ggthemr)
library(extrafont)

ggthemr(palette = "sea", type = "outer")
fonts()

commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

commute <- commute_mode %>%
  arrange(city) %>%
  group_by(state, mode) %>%
  summarise(avg_pct = mean(percent)) %>%
  ungroup() %>%
  spread(key = "mode", value = "avg_pct") %>%
  mutate(total_pct = Bike + Walk) %>%
  mutate(state = tolower(state), log_total_pct = log(total_pct))

states_map <- map_data("state")
commute_map <- left_join(states_map, commute, by = c("region" = "state"))

ggplot(commute_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = log_total_pct), color = "white") +
  ggtitle("Percentage of Walking + Biking Commuters by State") +
  scale_fill_continuous(low = "red", high = "green") +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.line = element_blank(), text = element_text(family = "Georgia")) +
  labs(fill = "Log of Total\nPercentage")

