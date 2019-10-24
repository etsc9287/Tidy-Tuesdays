library(tidyverse)
library(ggthemr)
library(extrafont)
library(lubridate)

ggthemr_reset()

horror <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

clean_horror <- horror %>%
  mutate(release_year = as.numeric(str_extract(title, "((\\d{4}))"))) %>%
  mutate(genres=strsplit(genres, "\\|")) %>% 
  unnest(genres) %>%
  filter(between(release_year, 2011, 2017), !is.na(genres))

top_genres <- clean_horror %>%
  group_by(genres) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(n = if_else(genres == "Horror", n + 1382, n + 0)) %>%
  filter(genres != "Horror", n != 1382) %>%
  head(10)

clean_horror2 <- clean_horror %>%
  group_by(release_year, genres) %>%
  summarise(count = n()) 

clean_horror2 %>%
  semi_join(top_genres, by = "genres") %>%
  ggplot(aes(release_year, genres)) +
  geom_tile(aes(fill = count)) +
  scale_fill_continuous(low = "blue", high = "black") +
  xlab("Release Year") +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  ylab("Genre") +
  ggtitle("Frequencies of Other Genres in Horror Movies Over Time") +
  theme(text = element_text(family = "Trattatello"))

fonts()

table(clean_horror$genres)
         
