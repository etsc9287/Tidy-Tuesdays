library(tidyverse)
library(ggimage)
library(gganimate)

nz_birds <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv") %>%
  filter(vote_rank == "vote_1") %>%
  group_by(date, bird_breed) %>%
  tally()

top_votes <- nz_birds %>%
  group_by(date) %>%
  summarise(max_votes = max(n))

top_birds <- nz_birds %>%
  semi_join(top_votes, by = c("date", "n" = "max_votes"))

top_birds %>%
  ggplot(aes(date, n, fill = bird_breed)) +
  geom_col() +
  xlab("Date") +
  ylab("Number of Votes") +
  ggtitle("The Top Bird For Each Day of the Voting Period", subtitle = "Day ") +
  scale_fill_manual(values = c("green", "yellow")) +
  theme_dark()
  