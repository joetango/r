
### PHC6791 Final Project ###
### Joseph Dickerson ###


trashwheel <- read.csv("C:/Users/Joe/git/r/phc6791/final-proj/trashwheel.csv")

library(tidyverse)
library(viridis)
library(paletteer)
library(gridExtra)

### Exploratory Analysis ###

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
  filter(ID != "gwynnda", ID !="captain", Year >= 2017) %>% 
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>% 
  mutate(Location = ifelse(ID == "mister", "Jones Falls", "Harris Creek"))

data %>% 
  ggplot(aes(x = Date, y = Weight, color = Location)) +
  geom_point(size = 1) +
  geom_smooth(fill = NA, size = 1) +
  coord_cartesian(ylim = c(0, 40)) +
  labs(title = "Total Weight of Collected Trash by Month",
       y = "Weight (tons)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  scale_color_manual(values = c("Harris Creek" = "#1f78b4",  
                                "Jones Falls" = "#ff7f00")) +
  theme_light()

data %>% 
  ggplot(aes(y = Weight, x = Location, color = Location)) +
  geom_boxplot() +
  scale_color_paletteer_d("ggsci::nrc_npg") +
  labs(title = "Total Weight of Collected Trash Since 2017",
       y = "Weight (tons)",
       x = "",
       caption = "Source: Waterfront Partnership of Baltimore") +
  scale_color_manual(values = c("Harris Creek" = "#1f78b4",  
                                "Jones Falls" = "#ff7f00")) +
  theme_light() +
  theme(legend.position = "none")
 
### Jones Falls Data Cleaning ###

data_mister <- trashwheel %>% 
  filter(ID == "mister", Year >= 2017) %>% 
  group_by(ID, Year, Month) %>% 
  summarize(PlasticBottles = sum(PlasticBottles),
            GlassBottles = sum(PlasticBottles),
            PlasticBags = sum(PlasticBags),
            Butts = sum(CigaretteButts),
            Poly = sum(Polystyrene)) %>% 
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

j1 <- data_mister %>% 
  ggplot(aes(x = Date, y = PlasticBags)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bags",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Jones Falls Plastic Bottles ###

j2 <- data_mister %>% 
  ggplot(aes(x = Date, y = PlasticBottles)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  scale_y_continuous(position = "right") +
  labs(title = "Plastic Bottles",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Jones Falls Cigarette Butts ###

j3 <- data_mister %>% 
  ggplot(aes(x = Date, y = Butts)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Cigarette Butts",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))


### Jones Falls Polystyrene ###

j4 <- data_mister %>% 
  ggplot(aes(x = Date, y = Poly)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  scale_y_continuous(position = "right") +
  labs(title = "Polystyrene",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Arrange ###

grid.arrange(j1, j2, j3, j4,
             ncol = 2,
             top = "Trash Collection Over Time in Jones Falls")

### Harris Creek Data Cleaning ###

data_prof <- trashwheel %>% 
  filter(ID == "professor") %>% 
  group_by(ID, Year, Month) %>% 
  summarize(PlasticBottles = sum(PlasticBottles),
            GlassBottles = sum(PlasticBottles),
            PlasticBags = sum(PlasticBags),
            Butts = sum(CigaretteButts),
            Poly = Polystyrene) %>% 
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

h1 <- data_prof %>% 
  ggplot(aes(x = Date, y = PlasticBags)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Plastic Bags",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Harris Creek Plastic Bottles ###

h2 <- data_prof %>% 
  ggplot(aes(x = Date, y = PlasticBottles)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  scale_y_continuous(position = "right") +
  labs(title = "Plastic Bottles",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Harris Creek Cigarette Butts ###

h3 <- data_prof %>% 
  ggplot(aes(x = Date, y = Butts)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  labs(title = "Cigarette Butts",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Harris Creek Polystyrene ###

h4 <- data_prof %>% 
  ggplot(aes(x = Date, y = Poly)) +
  geom_point(size = 0.9) +
  geom_smooth(fill = NA, color = "#4DBBD5") +
  scale_y_continuous(position = "right") +
  labs(title = "Polystyrene",
       y = "Count",
       x = "") +
  theme_light() +
  theme(plot.title = element_text(size = 10))

### Arrange ###

grid.arrange(h1, h2, h3, h4,
             ncol = 2,
             top = "Trash Collection Over Time in Harris Creek")
             
