# Question 5:
# 
# Apply boosting, bagging, random forests, and BART to a data set of your choice. 
# Be sure to fit the models on a training set and to evaluate their performance
# on a test set. How accurate are the results compared to simple methods like 
# linear or logistic regression? Which of these approaches yields the best 
# performance?


library(ISLR2)
library(randomForest)
library(BART)
data(Carseats)
set.seed(1)

### Bagging ###

train <- sample(1:nrow(Carseats), (nrow(Carseats)/2))

test <- Carseats[-train, ]

bag.Carseats <- randomForest(Sales ~ ., data = Carseats, subset = train,
                         mtry = (ncol(Carseats) - 1), importance = TRUE)

bag.Carseats

Carseats.test <- Carseats[-train, "Sales"]

yhat.bag <- predict(bag.Carseats, newdata = test)
plot(yhat.bag, test[,"Sales"])
abline(0, 1)

mean((yhat.bag - test[,"Sales"])^2) ## test set MSE: 2.624

### Random Forest ###

rf.Carseats <- randomForest(Sales ~ ., data = Carseats, subset = train,
                            mtry = round(sqrt(ncol(Carseats))),
                            importance = TRUE)
yhat.rf <- predict(rf.Carseats, newdata = test)

mean((yhat.rf - test[,"Sales"])^2) ## test set MSE: 3.001

plot(yhat.rf, test[,"Sales"])
abline(0, 1)

### BART ###

x <- Carseats[, 2:11]
y <- Carseats[, "Sales"]
xtrain <- x[train, ]
ytrain <- y[train]

xtest <- x[-train, ]
ytest <- y[-train]

bartfit <- gbart(xtrain, ytrain, x.test = xtest)

yhat.bart <- bartfit$yhat.test.mean

mean((ytest - yhat.bart)^2) ## test set MSE: 1.438

ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord] ## checking how many times each variable appeared
                           ## in the collection of trees

### Linear Model for Comparison ###

lmod <- lm(Sales ~ ., data = Carseats)
summary(lmod)

lm.preds <- predict(lmod)

mean((Carseats$Sales - lm.preds)^2)
