library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(tree)
library(e1071)


data <- read.csv("social-media.csv")

data <- data %>% 
  mutate(GenderNumeric = ifelse(Gender == "Male", 1, 0)) %>% 
  mutate(Affects_Academic_Performance_Numeric = ifelse(
    Affects_Academic_Performance == "No", 0, 1)) %>% 
    mutate(Mental_Health_Score_Class = ifelse(
      Mental_Health_Score <= 5, 0, 1
    ))

data <- data[, -5]

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

lmod <- glm(Mental_Health_Score ~ Age + GenderNumeric +
               Avg_Daily_Usage_Hours + 
               Affects_Academic_Performance_Numeric +
               Sleep_Hours_Per_Night +
               Avg_Daily_Usage_Hours +
               Addicted_Score,
               data = data, family = "gaussian")

summary(lmod)

## Logistic Model

glmod <- glm(Mental_Health_Score ~ 
               Avg_Daily_Usage_Hours +
               Sleep_Hours_Per_Night,
             data = data, family = "binomial")

summary(glmod)

## Random Forest

set.seed(1)

seq <- sample(1:nrow(data), (nrow(data)/2))
train <- data[seq, ]
test <- data[-seq, ]

rf.model <- randomForest(Mental_Health_Score ~ Avg_Daily_Usage_Hours +
                        Age + GenderNumeric + 
                        Affects_Academic_Performance_Numeric +
                        Sleep_Hours_Per_Night + Addicted_Score, 
                         data = train)

yhat.rf <- predict(rf.model, newdata = test)
mean((yhat.rf - test[,"Mental_Health_Score"])^2) ## test mse 0.0997
                                                  ## similar to rf.model summary

plot(yhat.rf, test[,"Mental_Health_Score"])
abline(0, 1)

## SVM

svm.fit <- svm(Mental_Health_Score_Class ~ ., data = train, kernel = "linear",
              cost = 0.01, scale = FALSE)

summary(svm.fit)

tune.out <- e1071::tune(svm, Mental_Health_Score_Class ~ .,
                        data = train, kernel = "linear",
                        ranges = list(cost = c(.01, 1, 2, 4, 6, 8, 10)))
summary(tune.out)
plot(tune.out)  ## cost = 1

svm.fit <- svm(Mental_Health_Score_Class ~ ., data = train, kernel = "linear",
               cost = 1, scale = FALSE)

summary(svm.fit)

yhat.svm <- predict(svm.fit, newdata = test, type = "class")
table(yhat.svm, test$Mental_Health_Score_Class)



