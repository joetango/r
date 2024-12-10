
require(faraway)
require(ggplot2)
require(dplyr)

data(rats)

## 1a 
rats %>% 
  ggplot(aes(x = poison, y = time)) +
  geom_point() +
  stat_summary(fun.y = "mean", geom = "line", aes(group = treat, color = treat)) +
  theme(legend.position = "top", legend.direction = "horizontal")

rats %>% 
  ggplot(aes(x = treat, y = time, shape = poison)) +
  geom_point() +
  stat_summary(fun.y = "mean", geom = "line",
               aes(group = poison, color = poison)) +
  theme(legend.position = "top", legend.direction = "horizontal")

## 1b
lmod <- lm(time ~ poison*treat, rats)
anova(lmod)

## 1c ??? poison 3 converging w treat b and d, different from i and ii

## 1d

qqnorm(residuals(lmod), main="")
qqline(residuals(lmod)) ## long-tailed errors

plot(fitted(lmod), residuals(lmod), xlab = "Fitted",
     ylab = "Residuals") ## symmetry
abline(h=0)

plot(residuals(lmod) ~ poison, rats, ylab = "Residuals")


## Test for difference in variance
ratse <- rats[(1:48),]
ratse$res <- sqrt(abs(residuals(lmod))[(1:48)])
vmod <- lm(res ~ poison + treat, ratse)
anova(vmod)

## 1e ???

## 1f

require(MASS)
bc <- boxcox(lmod)
lambda <- bc$x[which.max(bc$y)]

time_new <- (rats$time^lambda - 1) / lambda

lmod_main <- lm(time ~ poison + treat, rats)
lmod_transformed <- lm(time_new ~ poison + treat, rats)

summary(lmod_main)
summary(lmod_transformed)

qqnorm(residuals(lmod_transformed))
qqline(residuals(lmod_transformed))

## 2

## require(magic)
data(OrchardSprays)

xtabs(decrease ~ rowpos + colpos, OrchardSprays)
matrix(OrchardSprays$treatment,8,8,byrow = TRUE)

lmodLatin <- lm(decrease ~ treatment + as.factor(rowpos) + as.factor(colpos), OrchardSprays)
drop1(lmodLatin, test="F")
summary(lmodLatin)
qtukey(0.95, 8, 42)*9.757/sqrt(2) ## 8 is no. of treatments, 42 dof

scoefs <- c(0, coef(lmodLatin)[2:8])
outer(scoefs, scoefs, "-")

