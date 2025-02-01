library(tidyverse)

dataframe <- read.csv("phc6791/data_life_who.csv")
df_clean <- dataframe[-1,]

dfc <- df_clean %>% 
  group_by(X.1) %>% 
  summarise(Male = mean(as.integer(Life.expectancy.at.birth..years..1)),
            Female = mean(as.integer(Life.expectancy.at.birth..years..2))) 

dfl <- dfc %>% 
  pivot_longer(cols = c(Male, Female),
               names_to = "Sex", values_to = "life.expectancy") %>% 
  arrange(Sex)

dfl %>% ggplot(aes(x = X.1, y = life.expectancy, color = Sex, group = Sex)) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Worldwide Average Life Expectancy at Birth Over Time",
  x = "Year", y = "Life Expectancy (years)", color = "")
