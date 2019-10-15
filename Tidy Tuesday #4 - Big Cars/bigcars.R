library(tidyverse)
library(lubridate)
library(modelr)
library(extrafont)
library(ggthemr)
library(gganimate)
library(reshape2)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

ggthemr(palette = "chalk", type = "outer")
fonts()

cars <- big_epa_cars %>%
  select(make, model, trany, VClass, year, fuelType, fuelCost08, drive, highway08, youSaveSpend, co2, feScore, city08, co2) %>%
  filter(year >= 2015)

cars1 <- cars %>%
  group_by(make) %>%
  summarise(avgfuel = mean(fuelCost08), avgspend = mean(youSaveSpend), avghighway = mean(highway08), avgscore = mean(feScore), avgcity = mean(city08), n = n(), avgco2 = mean(co2)) %>%
  arrange(desc(n)) %>%
  filter(make == "Tesla" | n >= 200)

cars1 %>%
  ggplot(aes(fct_reorder(make, avgspend), avgspend)) +
  geom_point(aes(size = avghighway, color = avgco2)) +
  coord_flip() +
  xlab(NULL) +
  ylab("Average Save to Spend Ratio Over 5 yrs (USD)") +
  ggtitle("Which Car is the Best on the Market since 2015?", subtitle = "Comparing Teslas to the most common cars") +
  labs(size = "Average MPG / MPC", color = "Average CO2 Emissions\n(grams/mile)") +
  scale_color_continuous(low = "green", high = "red") +
  theme(text = element_text(family = "AppleMyungjo")) 

table(big_epa_cars$year)
