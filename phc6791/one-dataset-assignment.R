library(tidyverse)

dataframe <- read.csv("phc6791/data_life_who.csv")
df_clean <- dataframe[-1,]

dfc <- df_clean %>% 
  group_by(X.1) %>% 
  summarise(Male = mean(as.integer(Life.expectancy.at.birth..years..1)),
            Female = mean(as.integer(Life.expectancy.at.birth..years..2)),
            Male.HALE = mean(as.integer(Healthy.life.expectancy..HALE..at.birth..years..1)),
            Female.HALE = mean(as.integer(Healthy.life.expectancy..HALE..at.birth..years..2))) 

dfl <- dfc %>% 
  pivot_longer(cols = c(Male, Female, Male.HALE, Female.HALE),
               names_to = "Sex", values_to = "life.expectancy") %>% 
  arrange(Sex) %>% 
  mutate(LEvsHALE = ifelse(Sex == "Male" | Sex == "Female", "Life Expectancy",
                           "Healthy Life Expectancy"))

dfl %>% ggplot(aes(x = X.1, y = life.expectancy, color = Sex, group = Sex)) +
  geom_point(size = 2) +
  geom_line() +
  facet_grid(~ LEvsHALE) +
  labs(title = "Life Expectancy and Healthy Life Expectancy Over Time",
       subtitle = "Average across 183 countries", x = "", y = "Age in Years",
  caption = "World Health Organization. (2020). Life expectancy and Healthy
  life expectancy", color = "") 
