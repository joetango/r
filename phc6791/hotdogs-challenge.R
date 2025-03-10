hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")

library(tidyverse)

hotdogs <- hotdogs %>% mutate(
  is.usa = ifelse(Country == "United States", 1, 0)
)

hotdogs %>% 
  ggplot(aes(x = Year, y = Dogs.eaten, fill = factor(is.usa))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#cccccc", "#821122")) +
  theme_classic() +
  labs(x = "Year", y = "Hot dogs and buns (HDB) eaten") +
  scale_x_continuous(breaks = seq(1980, 2010, by = 4)) +
  scale_y_continuous(limits = c(0, max(hotdogs$Dogs.eaten)),
                     breaks = c(0, seq(10, 60, by = 10)),
                     labels = c(0, 10, "", 30, "", 50, "")) +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line())