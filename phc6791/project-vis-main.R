library(tidyverse)

## FL-508
## Gainesville/Alachua, Putnam Counties CoC


year <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015,
          2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007)

overall.homeless <- c(752, 1006, 925, 677, 880, 804, 756, 819, 844,
                      870, 1256, 1718, 1814, 1179, 1019, 924, 744, 678)

sheltered.total.homeless <- c(308, 262, 338, 243, 279, 306, 323, 373,
                              419, 317, 368, 413, 389, 439, 297, 301,
                              279, 263)

pct.sheltered <- (sheltered.total.homeless / overall.homeless) * 100

overall.sheltered.ratio

df <- data.frame(Year = year,
                 Overall.Homeless = overall.homeless,
                 Sheltered.Total.Homeless = sheltered.total.homeless,
                 Percent.Sheltered = pct.sheltered)



df %>% ggplot(aes(x = Year, y = Percent.Sheltered)) +
  geom_line() +
  scale_x_continuous(breaks = c(seq(2007, 2024, by = 3), 2024))
