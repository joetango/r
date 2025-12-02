library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(tree)
library(e1071)


## Read data

data <- read.csv("social-media.csv")

## Data cleaning

data <- data %>% 
  mutate(Gender = as.factor(Gender),
         Affects_Academic_Performance = 
           as.factor(Affects_Academic_Performance)) %>%
  select(-Country,
         -Most_Used_Platform,
         -Relationship_Status,
         -Conflicts_Over_Social_Media,
         -Addicted_Score,
         -Student_ID,
         -Academic_Level) %>% 
  slice(-705) 
  
AAP.numeric <- ifelse(data$Affects_Academic_Performance == "No", 0, 1)

## Exploratory plots

data %>% 
  ggplot(aes(y = Avg_Daily_Usage_Hours, x = Mental_Health_Score)) +
  geom_point() +
  geom_smooth(fill = NA, color = "red") +
  labs(title = "Average Daily Social Media Usage vs. Mental Health Score",
       y = "Average Daily Use (Hours)",
       x = "Mental Health Score") +
  theme_light() 

data %>%
  ggplot(aes(y = Avg_Daily_Usage_Hours, x = Affects_Academic_Performance)) +
  geom_boxplot() +
  labs(title = "Average Daily Social Media Usage and Academic Performance",
       y = "Average Daily Use (Hours)",
       x = "Affects Academic Performance") +
  theme_light()

## Linear model

lmod <- lm(Mental_Health_Score ~ ., data = data)
summary(lmod)


## Logistic Model

glmod <- glm(Affects_Academic_Performance ~ Age + 
               Gender + 
               Avg_Daily_Usage_Hours + 
               Sleep_Hours_Per_Night,
             data = data, family = "binomial")

summary(glmod)


## Random Forest  

set.seed(1)

seq <- sample(1:nrow(data), (nrow(data)/2))
train <- data[seq, ]
test <- data[-seq, ]

rf.model <- randomForest(Mental_Health_Score ~ ., data = train)

yhat.rf <- predict(rf.model, newdata = test)
mean((yhat.rf - test[,"Mental_Health_Score"])^2) ## test mse 0.1022
                                                  ## similar to rf.model summary

plot(yhat.rf, test[,"Mental_Health_Score"])
abline(0, 1)

## SVM Fit

tune.out <- e1071::tune(svm, Affects_Academic_Performance ~ .,
                        data = train, kernel = "linear",
                        ranges = list(cost = c(.01, 1, 2, 4, 6, 8, 10)))
summary(tune.out)
plot(tune.out)  ## cost = 1

svm.fit <- svm(Affects_Academic_Performance ~ .,
               data = train, kernel = "linear",
               cost = 1, scale = FALSE)

summary(svm.fit)

yhat.train.svm <- predict(svm.fit, newdata = train, type = "class")
mean(yhat.train.svm != 
       train$Affects_Academic_Performance) ## training error 0.0540


yhat.svm <- predict(svm.fit, newdata = test, type = "class")
mean(yhat.svm != test$Affects_Academic_Performance) ## test error 0.0369

table(yhat.svm, test$Affects_Academic_Performance) ##checking test error
(13) / (209 + 126 + 17) ## 0.0369 

