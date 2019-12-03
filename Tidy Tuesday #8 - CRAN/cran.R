library(tidyverse)
library(extrafont)
library(ggthemr)

ggthemr(palette = "chalk", type = "outer")
fonts()

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

lines_code <- cran_code %>%
  group_by(pkg_name) %>%
  summarise(lines = sum(code)) %>%
  arrange(desc(lines)) %>%
  head(10)

lines_code %>%
  ggplot(aes(fct_reorder(pkg_name, lines), lines)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 10 Most Heavily Coded R Packages") +
  xlab(NULL) +
  ylab("Lines of Code") +
  theme(legend.position = "none", text = element_text(family = "Impact")) +
  coord_flip()
