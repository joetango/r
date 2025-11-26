library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(tree)

data <- read.csv("social-media.csv")

data <- data %>% 
  mutate(GenderNumeric = ifelse(Gender == "Male", 1, 0)) %>% 
  mutate(Affects_Academic_Performance_Numeric = ifelse(
    Affects_Academic_Performance == "No", 0, 1
  ))

attach(data)

g1 <- data %>% 
  ggplot(aes(y = Avg_Daily_Usage_Hours, x = Addicted_Score)) +
    geom_point() +
    geom_smooth(fill = NA) +
    theme_minimal()

g2 <- data %>% 
  ggplot(aes(y = Avg_Daily_Usage_Hours, x = Mental_Health_Score)) +
  geom_point() +
  geom_smooth(fill = NA) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

data %>% 
  ggplot(aes(x = Gender, y = Avg_Daily_Usage_Hours)) +
  geom_boxplot()

data %>% 
  ggplot(aes(x = Most_Used_Platform, y = Avg_Daily_Usage_Hours)) +
  geom_boxplot()

data %>% 
  ggplot(aes(x = Most_Used_Platform, color = Gender)) +
  geom_bar()

  

grid.arrange(g1, g2, nrow = 1)

## Linear Model

lmod <- glm(Avg_Daily_Usage_Hours ~ Age + GenderNumeric +
               Avg_Daily_Usage_Hours + 
               Affects_Academic_Performance_Numeric +
               Sleep_Hours_Per_Night +
               Mental_Health_Score +
               Addicted_Score,
               data = data, family = "gaussian")

summary(lmod)

## Logistic Model

glmod <- glm(Affects_Academic_Performance_Numeric ~ 
               Avg_Daily_Usage_Hours +
               Sleep_Hours_Per_Night,
             data = data, family = "binomial")

summary(glmod)

## Random Forest

rf.model <- randomForest(Mental_Health_Score ~ Avg_Daily_Usage_Hours +
                        Age + GenderNumeric + 
                        Affects_Academic_Performance_Numeric +
                        Sleep_Hours_Per_Night + Addicted_Score, 
                         data = data)
rf.model

