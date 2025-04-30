trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-05/trashwheel.csv')

library(tidyverse)
library(viridis)
library(paletteer)
library(gridExtra)

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
  filter(ID != "gwynnda", ID !="captain", Year >= 2016) %>% 
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% 
  mutate(Trashwheel = ifelse(ID == "mister", "Jones Falls", "Harris Creek"))

data %>% 
  ggplot(aes(x = Date, y = Weight, color = Trashwheel)) +
  geom_point(size = .7) +
  geom_smooth(fill = NA) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_paletteer_d("ggsci::nrc_npg") +
  labs(title = "Total Weight of Trash Collected by Month",
       y = "Weight (tons)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  theme_minimal()
 
### Jones Falls Data Cleaning ###

data_mister <- trashwheel %>% 
  filter(ID == "mister", Year >= 2017) %>% 
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

### Jones Falls Plastic Bags ###

pb_mister <- data_mister %>% 
  ggplot(aes(x = Date, y = PlasticBags)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bags Collected Over Time In Jones Falls Stream",
       y = "Plastic Bags (count)",
       x = "") +
  theme_minimal()

### Jones Falls Plastic Bottles ###

data_mister %>% 
  ggplot(aes(x = Date, y = PlasticBottles)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bottles Collected Over Time In Jones Falls",
       y = "Plastic Bottles (count)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  theme_minimal()

### Harris Creek Data Cleaning ###

data_prof <- trashwheel %>% 
  filter(ID == "professor") %>% 
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

### Harris Creek Plastic Bags ###

pb_prof <- data_prof %>% 
  ggplot(aes(x = Date, y = PlasticBags)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bags Collected Over Time In Harris Creek",
       y = "Plastic Bags (count)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  theme_minimal()

### Harris Creek Plastic Bottles ###

data_prof %>% 
  ggplot(aes(x = Date, y = PlasticBottles)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bottles Collected Over Time In Harris Creek",
       y = "Plastic Bottles (count)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  theme_minimal()

             