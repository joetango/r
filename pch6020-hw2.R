library(tidyverse)

sum(54 + 20 + 60 + 21 + 82) 
sum(68 + 39 + 133 + 35 + 150)


rct.data <- data.frame(
  group = rep(1:12),
  patients = c(13, 47, 13, 37, 37,
               150, 48, 183, 141, 87,
               11, 12),
  response = c(11, 26, 6, 15, 13,
               46, 13, 42, 24, 10,
               1, 1)
)

rct.data <- rct.data %>% 
  mutate(pct.response = round((response/patients), 2),
         pct.response = (pct.response * 100))

rct.data[10, 4] <- 12

p <- sum(rct.data$response) / sum(rct.data$patients)

#SE of lecture example
sqrt(p*(1-p)/sum(rct.data$patients))

fnc <- function(p, n){
  sqrt(p*(1-p)/n)
}

hw.data <- data.frame(
  id = rep(1:5),
  patients = c(68, 39, 133, 35, 150),
  response = c(54, 20, 60, 21, 82)
)

hwp <- sum(hw.data$response) / sum(hw.data$patients)

fnc(hwp, sum(hw.data$patients))

## 2b.

sum((hw.data$response / hw.data$patients)) / 5

sum((rct.data$response / rct.data$patients)) / 12

b.fnc <- function(p) {
  N <- length(p)
  p_bar <- sum(p) / N
  sqrt( sum((p - p_bar)^2) / (N - 1) )
}

rct.p <- (rct.data$response / rct.data$patients)

b.fnc(rct.p) / sqrt(length(rct.p))

hw.p <- (hw.data$response / hw.data$patients)

b.fnc(hw.p) / sqrt(length(hw.p))


hw.p <- (hw.data$response / hw.data$patients)

c.fnc <- function(p, response, n) {
  N <- length(p)
  S2.p <- sum((p - mean(p))^2) / (N - 1)
  mean.within.var <- mean(p * (1 - p) / n)
  sigma2.pi <- S2.p - mean.within.var
  return(sigma2.pi)
}

c.fnc(hw.p, hw.data$response, hw.data$patients)

c.fnc(rct.p, rct.data$response, rct.data$patients)

