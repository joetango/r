library(faraway)
library(tidyverse)

data(seeds)

## add box variable
df <- seeds %>% 
  mutate(box = rep(1:8, each = 6))


## model
glmod <- glm(cbind(germ, 100 - germ) ~ moisture + covered + factor(box), family = binomial, df)

summary(glmod)
glmod_sum = summary(glmod)

## chi-sq test
pchisq(deviance(glmod), df.residual(glmod), lower = F)

pearsonstat = sum(residuals(glmod, "pearson")^2)

pchisq(pearsonstat, glmod_sum$df.residual, lower.tail = F)

## testing (nonlinear fitted vs residuals)
glmfitted <- fitted(glmod)
glmresiduals <- residuals(glmod)

plot(residuals(glmod) ~ fitted(glmod))
abline(h=0)

## unsatisfactory Q-Q plot
qqnorm(residuals(glmod))
qqline(residuals(glmod))