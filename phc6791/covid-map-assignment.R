library(sf)
library(tidyverse)
library(gridExtra)



eu <- read_sf("Europe_SWAsia.shp") %>%
  select(NAME, geometry) %>%
  mutate(name = tolower(NAME))

covid <- read.csv("owid-covid-data.csv")

covid0 <- covid %>% 
  filter(continent == "Europe") %>%
  select(location, date, people_fully_vaccinated_per_hundred) %>%
  drop_na() %>%
  group_by(location) %>%
  mutate(location = tolower(location), 
         date = as.Date(date,  format = "%m/%d/%Y")) %>%
  filter(date == max(date))%>%
  rename(name = location) %>%
  select(name, people_fully_vaccinated_per_hundred)

covid1 <- covid %>% 
  filter(continent == "Europe") %>%
  select(location, date, icu_patients_per_million) %>%
  drop_na() %>%
  group_by(location) %>%
  mutate(location = tolower(location), 
         date = as.Date(date,  format = "%m/%d/%Y")) %>%
  filter(date == max(date))%>%
  rename(name = location) %>%
  select(name, icu_patients_per_million)

mapdata0 <- left_join(eu, covid0, by = "name") %>% 
  filter(!is.na(people_fully_vaccinated_per_hundred))

mapdata1 <- left_join(eu, covid1, by = "name") %>% 
  filter(!is.na(icu_patients_per_million))

common_countries <- intersect(mapdata0$name, mapdata1$name)
mapdata0 <- mapdata0 %>% filter(name %in% common_countries)
mapdata1 <- mapdata1 %>% filter(name %in% common_countries)


m1 <- ggplot(mapdata0) + 
  geom_sf(aes(fill = people_fully_vaccinated_per_hundred))+
  coord_sf() +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  labs(title = "Covid Vaccinations Per Hundred") +
  guides(fill = guide_legend(title = NULL))
  

m2 <- ggplot(mapdata1) + 
  geom_sf(aes(fill = icu_patients_per_million))+
  coord_sf() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_minimal() +
  labs(title = "ICU Patients Per Million") +
  guides(fill = guide_legend(title = NULL))

grid.arrange(m1, m2)
