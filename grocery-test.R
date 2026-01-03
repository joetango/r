library(tidyverse)

set.seed(1)

n <- 100

junkprobs <- c(rep(1, 10), rep(2, 10), rep(4, 10), rep(3, 10))
junkprobs <- junkprobs / sum(junkprobs)

wf.prod.probs <- c(rep(1, 5), rep(2, 10), rep(3, 10), 
                   rep(6, 10), rep(5, 5), rep(1, 10))
wf.prod.probs <- wf.prod.probs / sum(wf.prod.probs)

data <- data.frame(
  store = c(rep("safeway", 45), rep("wholefoods", 30), rep("sprouts", 25)),
  dry.grocery = c(sample(10:150, 75, replace = TRUE)/.95,
                  sample(50:150, 25, replace = TRUE)/.95),
  produce = c(sample(1:40, 45, replace = TRUE)/.95,
              sample(11:60, 30, replace = TRUE, prob = wf.prod.probs)/.95, 
              sample(11:50, 25, replace = TRUE)/.95),
  misc = sample(0:30, n, replace = TRUE)/.95,
  junk = c((sample(1:40, 45, replace = TRUE, prob = junkprobs)/.95),
           (sample(1:40, 30, replace = TRUE)/.95),
           (sample(1:40, 25, replace = TRUE)/.95))
)

data <- data %>% 
  mutate(total = (dry.grocery + produce + misc + junk),
         total = round(total, 2),
         pct.junk = ((junk/total) * 100),
         pct.junk = round(pct.junk, 2),
         dry.grocery = round(dry.grocery, 2),
         produce = round(produce, 2),
         misc = round(misc, 2),
         junk = round(junk, 2))

lmod <- lm(pct.junk ~ store, data = data)
summary(lmod)


data %>% 
  ggplot(aes(y = pct.junk, color = store)) +
  geom_boxplot()

data %>% 
  ggplot(aes(y = produce, color = store)) +
  geom_boxplot()
