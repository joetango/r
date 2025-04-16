trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-05/trashwheel.csv')

library(tidyverse)
library(viridis)
library(paletteer)

data <- trashwheel %>% 
  group_by(ID, Year, Month) %>% 
  summarize(Weight = sum(Weight)) %>% 
  mutate(Month = recode(Month,
                       January = 1,
                       February = 2,
                       March = 3,
                       April = 4,
                       May = 5,
                       June = 6,
                       July = 7,
                       August = 8,
                       September = 9,
                       October = 10,
                       November = 11,
                       December = 12)) %>% 
  arrange(ID, Year, Month) %>% 
  filter(ID != "gwynnda", Year >= 2016) %>% 
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

data %>% 
  ggplot(aes(x = Date, y = Weight, color = ID)) +
  geom_point() +
  geom_smooth(fill = NA) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_paletteer_d("ggsci::nrc_npg") +
  labs(title = "Collected Trash Over Time",
       y = "Weight (tons)",
       x = "") +
  theme_minimal()
 

data_mister <- trashwheel %>% 
  filter(ID == "mister") %>% 
  group_by(ID, Year, Month) %>% 
  summarize(PlasticBottles = sum(PlasticBottles),
            GlassBottles = sum(PlasticBottles),
            PlasticBags = sum(PlasticBags)) %>% 
  mutate(Month = recode(Month,
                        January = 1,
                        February = 2,
                        March = 3,
                        April = 4,
                        May = 5,
                        June = 6,
                        July = 7,
                        August = 8,
                        September = 9,
                        October = 10,
                        November = 11,
                        December = 12)) %>% 
  arrange(ID, Year, Month) %>% 
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))

data_mister %>% 
  ggplot(aes(x = Date, y = PlasticBags)) +
  geom_point() +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bags Collected Over Time",
       y = "Plastic Bags (count)",
       x = "") +
  theme_minimal()
