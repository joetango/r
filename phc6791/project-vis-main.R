library(tidyverse)
library(gridExtra)
library(scales)

## FL-508
## Gainesville/Alachua, Putnam Counties CoC


year <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015,
          2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007)

overall.homeless <- c(752, 1006, 925, 677, 880, 804, 756, 819, 844,
                      870, 1256, 1718, 1814, 1179, 1019, 924, 744, 678)

sheltered.total.homeless <- c(308, 262, 338, 243, 279, 306, 323, 373,
                              419, 317, 368, 413, 389, 439, 297, 301,
                              279, 263)

below.poverty.pct <- c(NA, 19.7, 20.2, 20.1, 20.7, 21.4, 21.8, 23.3,
                       24.2, 24.3, 25.4, 24.9, 23.8, 23.6, 23.6, 24.1,
                       NA, NA)

unemployment.pct <- c(NA, 4.3, 5.1, 4.8, 5.0, 5.1, 6.1, 6.6, 7.0, 7.9,
                      8.5, 8.6, 7.9, 7.3, 6.7, 6.7, NA, NA)

pct.sheltered <- (sheltered.total.homeless / overall.homeless) * 100

df <- data.frame(Year = year,
                 Overall.Homeless = overall.homeless,
                 Sheltered.Total.Homeless = sheltered.total.homeless,
                 Percent.Sheltered = pct.sheltered,
                 Poverty.Pct = below.poverty.pct,
                 Unemployment.Pct = unemployment.pct)


df1 <- df %>% pivot_longer(cols = c(Poverty.Pct, Unemployment.Pct),
                           names_to = "Variable",
                           values_to = "Percent") 

df1 <- na.omit(df1)

plot0 <- df %>% 
  filter(!is.na(Poverty.Pct)) %>% 
  ggplot(aes(x = Year, y = Poverty.Pct)) +
  geom_line(color = "#ce89dc", size = 1) +
  geom_point() +
  scale_x_continuous(breaks = c(seq(2009, 2023, by = 5), 2023)) +
  scale_y_continuous(limits = c(19.5, 26),
                     label = scales::percent_format(scale = 1)) +
  labs(title = "Individuals Under Poverty Line in Alachua County",
       y = "Percentage of Population",
       caption = "Source: United States Bureau of the Census") +
  theme_classic() 

plot3 <- df %>% ggplot(aes(x = Year, y = Overall.Homeless)) +
  geom_line(color = "#ce89dc", size = 1) +
  geom_point() +
  scale_x_continuous(breaks = c(seq(2009, 2023, by = 5), 2023),
                     limits = c(2009, 2023)) +
  labs(title = "Overall Homeless Count in Alachua/Putnam County",
       y = "Overall Homeless Count",
       caption = "Source: HUD 2007-2024 Point-in-Time Estimates by CoC") +
  theme_classic() 

grid.arrange(plot0, plot3)

# plot1 <- df1 %>% ggplot(aes(x = Year, y = Percent, color = Variable)) +
#   geom_line() +
#   scale_x_continuous(breaks = c(seq(2009, 2023, by = 5), 2023)) +
#   scale_y_continuous(limits = c(0, 30)) +
#   labs(title = "Poverty and Unemployment Over Time in Alachua County",
#        y = "Percentage",
#        caption = "Source: United States Bureau of the Census") +
#   theme_classic() 
# 
# plot2 <- df %>% ggplot(aes(x = Year, y = Overall.Homeless)) +
#   geom_rect(aes(xmin = 2009, xmax = 2023, ymin = -Inf, ymax = Inf), 
#            fill = "lightgrey", alpha = 0.02) +
#   geom_point() +
#   geom_smooth(fill = NA, color = "#ce89dc") +
#   scale_x_continuous(breaks = c(seq(2009, 2023, by = 5), 2023)) +
#   labs(title = "Overall Homelessness in Alachua/Putnam County",
#        y = "Overall Homeless Count",
#        caption = "Source: HUD 2007-2024 Point-in-Time Estimates by CoC") +
#   theme_classic() 

## grid.arrange(plot1, plot2)
