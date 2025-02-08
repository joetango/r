library(faraway)
library(MASS)
library(survival)
library(tidyr)
library(ggplot2)

########## 4.2 Link functions ##########
# data set measuring death rates for insects at different insectiside dosages
bliss

# fit binomial regressions with different link functions
mlogit <- glm(cbind(dead,alive)~conc,family=binomial(link = 'logit'), bliss)
mprobit <- glm(cbind(dead,alive)~conc,family=binomial(link = 'probit'), bliss)
mcloglog <- glm(cbind(dead,alive)~conc,family=binomial(link = 'cloglog'), bliss)
mcauchit <- glm(cbind(dead,alive)~conc,family=binomial(link = 'cauchit'), bliss)

# generate fitted values
predict(mlogit) # linear predictor scale
predict(mlogit, type = 'response') # response scale

# compare coefficients across models
# somewhat different, but have different meanings
sumary(mlogit)
sumary(mprobit)
sumary(mcloglog)
sumary(mcauchit)

# compare fitted values (all are similar)
predval = sapply(list(mlogit, mprobit, mcloglog, mcauchit), fitted)
dimnames(predval) = list(0:4, c('logit', 'probit', 'cloglog', 'cauchit'))
round(predval, 3)

# look at fitted curves over wider range of dosages
# main difference is at the ends for probabilities near 0 or 1
dose = seq(-4, 8, 0.2)
predval = sapply(list(mlogit, mprobit, mcloglog, mcauchit), 
                 function(m) predict(m, data.frame(conc = dose), type = 'response'))
colnames(predval) = c('logit', 'probit', 'cloglog', 'cauchit')
predval = data.frame(dose, predval)
mpv = gather(predval, link, probability, -dose)
ggplot(mpv, aes(x = dose, y = probability, color = link)) + geom_line()

########## 4.4 Prediction with Effective Doses ##########
lmod = glm(cbind(dead, alive) ~ conc, family = binomial, data = bliss)
lmodsum = summary(lmod)

# prediction and 95% confidence interval at concentration of 2.5
lmod_pred = predict(lmod, newdata = data.frame(conc=2.5), se = T)
# point estimate for predicted probability
predict(lmod, newdata = data.frame(conc=2.5), se = T, type = 'response')
# 95% CI for predicted probability
ilogit(lmod_pred$fit + c(-1,1)*1.96*lmod_pred$se.fit)

# prediction and 95% confidence interval at concentrations of -5
lmod_pred = predict(lmod, newdata = data.frame(conc=-5), se = T)
# point estimate and standard error for predicted probability
predict(lmod, newdata = data.frame(conc=-5), se = T, type = 'response')
# 95% CI for predicted probability
ilogit(lmod_pred$fit + c(-1,1)*1.96*lmod_pred$se.fit)

# lethal dose for 50%
# get 95% CI using delta method
ld50 = -lmod$coefficients[1] / lmod$coefficients[2]
dr = c(-1/lmod$coefficients[2], lmod$coefficients[1]/lmod$coefficients[2]^2)
se_ld50 = sqrt(dr %*% lmodsum$cov.unscaled %*% dr)[,]
2 + c(-1,1)*1.96*se_ld50

# built-in function to get doses & SEs from MASS package
dose.p(lmod, p = c(0.5, 0.9))

########## 4.3 Prospective and Retrospective Sampling ##########

# in prospective study, want to see the effect of food on disease
# log-odds of disease for boys comparing bottle vs breast-fed are
log( (77 / 381) / (47 / 447) )

# in retrospective study, want to compare rates of feeding type after 
# separating into cases/controls
# log-odds of bottle-feeding for boys comparing sick vs healthy are
log( (77 / 47) / (381 / 447) )

# both designs give the same results because of the mathematical 
# form of the log odds. Would not be true for, e.g., probit
qnorm(77/(381+77)) - qnorm(47/(447+47))
qnorm(77/(47+77)) - qnorm(381/(447+381))

# toy simulation to demonstrate effect on model intercept
# hypothetical prospective study (unbiased baseline rate estimation)
x = rnorm(n = 1000)
y = rbinom(n = 1000, size = 1, prob = ilogit(-3 + 2*x))
glm(y ~ x, family = binomial)

# hypothetical retrospective study where individuals are more likely to be 
# selected if they are cases
cases = which(y == 1)
controls = which(y == 0)
sampled_controls = sample(controls, length(cases))
y2 = c(y[cases], y[sampled_controls])
x2 = c(x[cases], x[sampled_controls])
glm(y2 ~ x2, family = binomial)

# coefficient estimate for X is similar, but large different in
# intercept as it reflects the balance of cases and controls in the sample

########## 4.5 Matched Case-Control Studies ##########
# matched case-control study examining relationship between X-rays
# and childhood AML (type of leukemia). Matched based on age, race, and county
head(amlxray)

# remove individuals with Downs syndrome because they are all cases in this
# data set and would result in an infinite coefficient estimate
amlxray[amlxray$downs == 'yes', 1:4]
ii = which(amlxray$downs == 'yes')
ramlxray = amlxray[-c(ii, ii+1),] #removes these individuals and their matched pairs

# conditional logistic regression treating CnRay as ordinal with cubic contrasts
cmod = clogit(disease ~ Sex + Mray + Fray + CnRay + strata(ID), ramlxray)
summary(cmod)

# conditional logistic regression with only linear contrast for CnRay
cmodr = clogit(disease ~ Fray + unclass(CnRay) + strata(ID), ramlxray)
summary(cmodr)

# standard logistic regression ignoring matched pairs (incorrect analysis!)
# parameter estimates are different
gmod = glm(disease ~ Fray + unclass(CnRay), family = binomial, ramlxray)
summary(gmod)
