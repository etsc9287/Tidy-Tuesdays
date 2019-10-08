library(tidyverse)
library(ggthemr)
library(lubridate)
library(gganimate)
library(extrafont)
library(reshape2)
library(scales)
install.packages("gifski")
library(gifski)

weights <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

#every row represents the placement of a participant in the event for a certain weight class.

#Question: Disqualifications over time

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

ggthemr(palette = "camoflauge", type = "inner", layout = "scientific")
fonts()

dqs <- weights %>%
  mutate(year = year(date)) %>%
  group_by(year, place) %>%
  summarise(dqs = n()) %>%
  ungroup() %>%
  complete(year, place, fill = list(dqs = 0)) %>%
  filter(place == "DQ")

dds <- weights %>%
  mutate(year = year(date)) %>%
  group_by(year, place) %>%
  summarise(dds = n()) %>%
  ungroup() %>%
  complete(year, place, fill = list(dds = 0)) %>%
  filter(place == "DD")

other <- weights %>%
  mutate(year = year(date)) %>%
  filter(place != "DD", place != "DQ") %>%
  group_by(year) %>%
  summarise(other = n())

joined <- inner_join(dqs, dds, by = "year") %>%
  inner_join(other, by = "year") %>%
  select(year, dqs, dds, other) %>%
  mutate(dq_prop = (dqs + dds) / (dqs + other + dds), dd_prop = dds / (dds + other + dqs))

graph1 <- joined %>%
  ggplot(aes(year, dq_prop)) +
  geom_line(color = "white") +
  geom_point(color = "white") +
  xlab("Year") +
  ylab("DQ Proportion") +
  ggtitle("Proportion of DQs in the IPL", subtitle = "Labels represent occurances") +
  scale_x_continuous(breaks = c(1973,1980,1990,2000,2010,2019)) +
  scale_y_continuous(labels = percent_format()) +
  theme(text = element_text(family = "Impact")) +
  geom_label(aes(label = if_else(year %in% c(1973, 1985, 1989, 1997,2011, 2019), dqs, NULL)))

graph2 <- joined %>%
  ggplot(aes(year, dd_prop)) +
  geom_line(color = "white") +
  geom_point(color = "white") +
  xlab("Year") +
  ylab("DD Proportion") +
  ggtitle("Proportion of Drug DQs in the IPL", subtitle = "Labels represent occurances") +
  scale_x_continuous(breaks = c(1973,1980,1990,2000,2010,2019)) +
  scale_y_continuous(labels = percent_format()) +
  theme(text = element_text(family = "Impact")) +
  geom_label(aes(label = if_else(year %in% c(1985,1989,1997,2006, 2019), dds, NULL)))

multiplot(graph1, graph2, cols = 2)
